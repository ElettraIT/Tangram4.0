       Identification Division.
       Program-Id.                                 mppssf             .
      *================================================================*
      *                                                                *
      *    Modulo per il trattamento del file [pss] per il sottosis-   *
      *    tema di stampa                                              *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Apertura modulo trattamento file [pss]             *
      *                                                                *
      *             Input  : j-ope = "OP"                              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              "##"      : errore grave          *
      *                                                                *
      *                      j-msg = messaggio di errore gia' emesso   *
      *                              se errore grave                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Chiusura modulo trattamento file [pss]             *
      *                                                                *
      *             Input  : j-ope = "CL"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Cancel      Test se modulo trattamento file [pss] sottoponibi- *
      *             le a cancellazione                                 *
      *                                                                *
      *             Input  : j-ope = "X?"                              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : modulo cancellabile   *
      *                              "##"      : modulo non cancell.   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Norm        Normalizzazione record file [pss]                  *
      *                                                                *
      *             Input  : j-ope = "NO"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *             Output : j-dat = dati record normalizzati          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Start       Start su file [pss]                                *
      *                                                                *
      *             Input  : j-ope = "ST"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *                      j-kre = tipo stampante          per "TST" *
      *                              codice stampante        per "STP" *
      *                              codice modulo           per "MOD" *
      *                              sigla fase gestionale   per "PMS" *
      *                              canale di uscita        per "IMP" *
      *                              codice stampante        per "MMO" *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-not-fnd : start errata          *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-not-fnd        *
      *                                                                *
      *             Nota   : La start viene comunque eseguita con il   *
      *                      tipo di confronto 'Not less'              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Read        Read next su file [pss]                            *
      * Next                                                           *
      *             Input  : j-ope = "RN"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-end-fil : fine fisica o logica  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-end-fil        *
      *                                                                *
      *             Nota   : La read next viene comunque eseguita sen- *
      *                      za lock                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Read        Lettura record file [pss]                          *
      *                                                                *
      *             Input  : j-ope = "RD"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *                      j-kre = tipo stampante          per "TST" *
      *                              codice stampante        per "STP" *
      *                              codice modulo           per "MOD" *
      *                              sigla fase gestionale   per "PMS" *
      *                              canale di uscita        per "IMP" *
      *                              codice stampante        per "MMO" *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-not-fnd : record non esistente  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-not-fnd        *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put         Scrittura record file [pss]                        *
      *                                                                *
      *             Input  : j-ope = "PT"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *                      j-kre = tipo stampante          per "TST" *
      *                              codice stampante        per "STP" *
      *                              codice modulo           per "MOD" *
      *                              sigla fase gestionale   per "PMS" *
      *                              canale di uscita        per "IMP" *
      *                              codice stampante        per "MMO" *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-dup-key : record gia' esistente *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-dup-key        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Update      Riscrittura record file [pss]                      *
      *                                                                *
      *             Input  : j-ope = "UP"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *                      j-kre = tipo stampante          per "TST" *
      *                              codice stampante        per "STP" *
      *                              codice modulo           per "MOD" *
      *                              sigla fase gestionale   per "PMS" *
      *                              canale di uscita        per "IMP" *
      *                              codice stampante        per "MMO" *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-not-fnd : record non esistente  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-not-fnd        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Delete      Delete record file [pss]                           *
      *                                                                *
      *             Input  : j-ope = "DE"                              *
      *                                                                *
      *                      j-tre = "TST" per tipo stampante          *
      *                              "STP" per codice stampante        *
      *                              "MOD" per codice modulo           *
      *                              "PMS" per parametri stampa        *
      *                              "IMP" per impegno stampante       *
      *                              "MMO" per modulo montato          *
      *                                                                *
      *                      j-kre = tipo stampante          per "TST" *
      *                              codice stampante        per "STP" *
      *                              codice modulo           per "MOD" *
      *                              sigla fase gestionale   per "PMS" *
      *                              canale di uscita        per "IMP" *
      *                              codice stampante        per "MMO" *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                                                                *
      *             -------------------------------------------------- *
      *             ___Da qui in poi da controllare___                 *
      *             -------------------------------------------------- *
      *                                                                *
      * Release     Rilascio preimpegno o impegno di una stampante     *
      *                                                                *
      *             Input  : j-ope = "00"                              *
      *                                                                *
      *                      j-kre = canale di uscita da rilasciare    *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : rilascio eseguito     *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore gia' emesso   *
      *                              se errore grave                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Preimpegno  Preimpegno di una stampante                        *
      *                                                                *
      *             Input  : j-ope = "01"                              *
      *                                                                *
      *                      j-kre = canale di uscita da preimpegnare  *
      *                                                                *
      *                      r-env = area environment                  *
      *                                                                *
      *                      r-ide = area identificazione              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : preimpegno  ottenuto  *
      *                              e-use-err : preimp. non ottenuto  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-use-err        *
      *                                                                *
      *                      j-dat = dati record impegno               *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Impegno OM  Impegno di una stampante per operazioni di manu-   *
      *             tenzione sulla stampante                           *
      *                                                                *
      *             Input  : j-ope = "02"                              *
      *                                                                *
      *                      j-kre = canale di uscita da impegnare     *
      *                                                                *
      *                      r-env = area environment                  *
      *                                                                *
      *                      r-ide = area identificazione              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : impegno ottenuto      *
      *                              e-use-err : impegno non ottenuto  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-use-err        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Impegno MM  Impegno di una stampante per operazioni di montag- *
      *             gio o smontaggio modulo o nastro stampa            *
      *                                                                *
      *             Input  : j-ope = "03"                              *
      *                                                                *
      *                      j-kre = canale di uscita da impegnare     *
      *                                                                *
      *                      r-env = area environment                  *
      *                                                                *
      *                      r-ide = area identificazione              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : impegno ottenuto      *
      *                              e-use-err : impegno non ottenuto  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-use-err        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Impegno SS  Impegno di una stampante per preparazione ad un    *
      *             servizio di spool                                  *
      *                                                                *
      *             Input  : j-ope = "04"                              *
      *                                                                *
      *                      j-kre = canale di uscita da impegnare     *
      *                                                                *
      *                      r-env = area environment                  *
      *                                                                *
      *                      r-ide = area identificazione              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : impegno ottenuto      *
      *                              e-use-err : impegno non ottenuto  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-use-err        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Impegno ST  Impegno di una stampante per esecuzione di una fa- *
      *             se di stampa                                       *
      *                                                                *
      *             Input  : j-ope = "I5"                              *
      *                                                                *
      *                      j-kre = canale di uscita da impegnare     *
      *                                                                *
      *                      r-env = area environment                  *
      *                                                                *
      *                      r-ide = area identificazione              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : impegno ottenuto      *
      *                              e-use-err : impegno non ottenuto  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-use-err        *
      *                                                                *
      *             -------------------------------------------------- *
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
      *    * File Control [pss]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pss   assign to disk       f-pss-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pss-key
                             file status  is            f-pss-sts     .
       

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *================================================================*
      *    File Description [pss]                                      *
      *----------------------------------------------------------------*
       fd  pss       label record standard                            .
       01  pss-rec.
           05  pss-key.
               10  pss-tre                pic  x(04)                  .
               10  pss-kre                pic  x(40)                  .
           05  pss-dat.
               10  pss-chr.
                   15  filler      occurs 1536
                                          pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work area per trattamento [pss] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-pss.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-pss-nam                  pic  x(04) value "pss "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-pss-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-pss-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .
       
      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work area per contatore di Open in corso                  *
      *    *-----------------------------------------------------------*
       01  w-opn.
      *        *-------------------------------------------------------*
      *        * COntatore Open in corso                               *
      *        *-------------------------------------------------------*
           05  w-opn-ctr                  pic  9(03) value zero       .

      *    *===========================================================*
      *    * Work per records di [pss] 'tst'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpsstst0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'mod'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssmod0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'stp'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssstp0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'imp'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssimp0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'mmo'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssmmo0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'pms'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpsspms0.cpw"                   .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Work per composizione messaggio di errore             *
      *        *-------------------------------------------------------*
           05  w-msg                      pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Contatore                                             *
      *        *-------------------------------------------------------*
           05  w-ctr                      pic  9(04)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

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

      ******************************************************************
       Procedure Division                using r
                                               j                      .
      ******************************************************************

      *================================================================*
      *       Declaratives                                             *
      *----------------------------------------------------------------*
       Declaratives.
       Decl Section.
           Use after standard error procedure on pss.
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status ritornato  *
      *              * dal cobol nel codice di i-o status convenziona- *
      *              * le                                              *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-pss                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-pss-sts            to   e-sts                  .
       End Declaratives.

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione return status code e message    *
      *              *-------------------------------------------------*
           move      spaces               to   j-rsc
                                               j-msg                  .
      *              *-------------------------------------------------*
      *              * Test su tipo operazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura modulo                             *
      *                  *---------------------------------------------*
           if        j-ope                =    "OP"
                     perform opn-mod-000  thru opn-mod-999
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           else if   j-ope                =    "CL"
                     perform cls-mod-000  thru cls-mod-999
      *                  *---------------------------------------------*
      *                  * Test se modulo cancellabile                 *
      *                  *---------------------------------------------*
           else if   j-ope                =    "X?"
                     perform tst-cnc-000  thru tst-cnc-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           else if   j-ope                =    "NO"
                     perform nor-rec-000  thru nor-rec-999
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           else if   j-ope                =    "ST"
                     perform str-rec-000  thru str-rec-999
      *                  *---------------------------------------------*
      *                  * Read next                                   *
      *                  *---------------------------------------------*
           else if   j-ope                =    "RN"
                     perform nxt-rec-000  thru nxt-rec-999
      *                  *---------------------------------------------*
      *                  * Read record                                 *
      *                  *---------------------------------------------*
           else if   j-ope                =    "RD"
                     perform rea-rec-000  thru rea-rec-999
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           else if   j-ope                =    "PT"
                     perform put-rec-000  thru put-rec-999
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           else if   j-ope                =    "UP"
                     perform upd-rec-000  thru upd-rec-999
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           else if   j-ope                =    "DE"
                     perform del-rec-000  thru del-rec-999
      *                  *---------------------------------------------*
      *                  * Preimpegno stampante                        *
      *                  *---------------------------------------------*
           else if   j-ope                =    "01"
                     perform pim-stp-000  thru pim-stp-999
      *                  *---------------------------------------------*
      *                  * Impegno stampante per operazioni di manu-   *
      *                  * tenzione                                    *
      *                  * Impegno stampante per operazioni di montag- *
      *                  * gio o smontaggio modulo o nastro stampa     *
      *                  * Impegno stampante per fase di esecuzione di *
      *                  * stampa                                      *
      *                  * Impegno stampante per preparazione a servi- *
      *                  * zio di spool                                *
      *                  *---------------------------------------------*
           else if   j-ope                =    "02"     or
                     j-ope                =    "03"     or
                     j-ope                =    "05"     or
                     j-ope                =    "04"
                     perform imp-stp-000  thru imp-stp-999
      *                  *---------------------------------------------*
      *                  * Rilascio preimpegno o impegno stampante     *
      *                  *---------------------------------------------*
           else if   j-ope                =    "00"
                     perform rel-stp-000  thru rel-stp-999            .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Apertura modulo                                           *
      *    *-----------------------------------------------------------*
       opn-mod-000.
      *              *-------------------------------------------------*
      *              * Se contatore di Open in corso maggiore di zero  *
      *              * Open non necessaria                             *
      *              *-------------------------------------------------*
           if        w-opn-ctr            not  = zero
                     go to opn-mod-400.
      *              *-------------------------------------------------*
      *              * Open i-o [pss]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione error-code                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [pss]             *
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
           move      "pss"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pss-pat              .
      *                  *---------------------------------------------*
      *                  * Operazione di Open                          *
      *                  *---------------------------------------------*
           open      i-o    pss                                       .
      *              *-------------------------------------------------*
      *              * Test se errori in open i-o                      *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to opn-mod-500.
       opn-mod-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore di Open in corso           *
      *              *-------------------------------------------------*
           add       1                    to   w-opn-ctr              .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-mod-999.
       opn-mod-500.
      *              *-------------------------------------------------*
      *              * Se errori in open i-o                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      "##"                 to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      "Sottosistema di stampa non disponibile !"
                                          to   w-msg                  .
           move      w-msg                to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-msg                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in on                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       opn-mod-600.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to opn-mod-800.
           if        v-alf                not  = "OK"
                     go to opn-mod-600.
       opn-mod-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       opn-mod-999.
           exit.

      *    *===========================================================*
      *    * Chiusura modulo                                           *
      *    *-----------------------------------------------------------*
       cls-mod-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore di Open in corso           *
      *              *-------------------------------------------------*
           subtract  1                    from w-opn-ctr              .
      *              *-------------------------------------------------*
      *              * Se contatore di Open in corso a zero : Close    *
      *              *-------------------------------------------------*
           if        w-opn-ctr            not  = zero
                     go to cls-mod-999.
           close     pss                                              .
       cls-mod-999.
           exit.

      *    *===========================================================*
      *    * Test se modulo cancellabile                               *
      *    *-----------------------------------------------------------*
       tst-cnc-000.
      *              *-------------------------------------------------*
      *              * Se il numero di Open in corso e' zero il modulo *
      *              * e' cancellabile, altrimenti non lo e'           *
      *              *-------------------------------------------------*
           if        w-opn-ctr            =    zero
                     move  spaces         to   j-rsc
           else      move  "##"           to   j-rsc                  .
       tst-cnc-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record                                    *
      *    *-----------------------------------------------------------*
       nor-rec-000.
      *              *-------------------------------------------------*
      *              * Test su tipo record                             *
      *              *-------------------------------------------------*
           if        j-tre                =    "TST "
                     perform nor-tst-000  thru nor-tst-999
                     move    w-tst        to   j-dat
           else if   j-tre                =    "STP "
                     perform nor-stp-000  thru nor-stp-999
                     move    w-stp        to   j-dat
           else if   j-tre                =    "MOD "
                     perform nor-mod-000  thru nor-mod-999
                     move    w-mod        to   j-dat
           else if   j-tre                =    "PMS "
                     perform nor-pms-000  thru nor-pms-999
                     move    w-pms        to   j-dat
           else if   j-tre                =    "IMP "
                     perform nor-imp-000  thru nor-imp-999
                     move    w-imp        to   j-dat
           else if   j-tre                =    "MMO "
                     perform nor-mmo-000  thru nor-mmo-999
                     move    w-mmo        to   j-dat                  .
       nor-rec-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "tst "                     *
      *    *-----------------------------------------------------------*
       nor-tst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-tst-cod-tst
                                               w-tst-des-tst
                                               w-tst-drv-tst          .
           move      zero                 to   w-tst-l72-tst
                                               w-tst-a72-tst
                                               w-tst-msu-tst
                                               w-tst-min-tst
                                               w-tst-msi-tst
                                               w-tst-mde-tst          .
           move      zero                 to   w-ctr                  .
       nor-tst-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    20
                     go to nor-tst-200.
           move      zero                 to   w-tst-spa-ori (w-ctr)
                                               w-tst-spa-ver (w-ctr)  .
           go to     nor-tst-100.
       nor-tst-200.
           move      spaces               to   w-tst-cla-tst          .
           move      spaces               to   w-tst-scl-tst          .
           move      zero                 to   w-ctr                  .
       nor-tst-300.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    10
                     go to nor-tst-400.
           move      spaces               to   w-tst-par-pds (w-ctr)  .
           go to     nor-tst-300.
       nor-tst-400.
           move      zero                 to   w-tst-nsu-tst          .
           move      zero                 to   w-tst-nin-tst          .
           move      zero                 to   w-tst-nsi-tst          .
           move      zero                 to   w-tst-nde-tst          .
           move      zero                 to   w-tst-pn1-tst          .
           move      zero                 to   w-tst-pn2-tst          .
           move      zero                 to   w-tst-pn3-tst          .
           move      zero                 to   w-tst-pn4-tst          .
           move      spaces               to   w-tst-pa1-tst          .
           move      spaces               to   w-tst-pa2-tst          .
           move      spaces               to   w-tst-pa3-tst          .
           move      spaces               to   w-tst-pa4-tst          .
           go to     nor-tst-999.
       nor-tst-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "stp "                     *
      *    *-----------------------------------------------------------*
       nor-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-stp-cod-stp
                                               w-stp-des-stp
                                               w-stp-tip-stp
                                               w-stp-can-stp
                                               w-stp-pwd-stp
                                               w-stp-pwd-mms
                                               w-stp-ibz-fff
                                               w-stp-des-ubi
                                               w-stp-sts-stp          .
       nor-stp-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "mod "                     *
      *    *-----------------------------------------------------------*
       nor-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-mod-cod-mod
                                               w-mod-des-mod
                                               w-mod-tip-mod          .
           move      zero                 to   w-mod-l72-mod
                                               w-mod-a72-mod          .
           move      zero                 to   w-ctr                  .
       nor-mod-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    20
                     go to nor-mod-200.
           move      zero                 to   w-mod-int-mod (w-ctr)  .
           go to     nor-mod-100.
       nor-mod-200.
           move      spaces               to   w-mod-cla-mod          .
           move      spaces               to   w-mod-sap-mod          .
       nor-mod-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "pms "                     *
      *    *-----------------------------------------------------------*
       nor-pms-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pms-fas-ges
                                               w-pms-cod-stp
                                               w-pms-out-stp
                                               w-pms-cod-mod          .
           move      zero                 to   w-pms-amp-car
                                               w-pms-alt-int          .
       nor-pms-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "imp "                     *
      *    *-----------------------------------------------------------*
       nor-imp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-imp-can-stp          .
           move      zero                 to   w-imp-sts-imp          .
           move      spaces               to   w-imp-azi-imp
                                               w-imp-ter-imp
                                               w-imp-ute-imp
                                               w-imp-sap-imp
                                               w-imp-arg-imp
                                               w-imp-set-imp
                                               w-imp-fas-imp          .
           move      zero                 to   w-imp-sdt-imp          .
       nor-imp-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "mmo "                     *
      *    *-----------------------------------------------------------*
       nor-mmo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-mmo-cod-stp
                                               w-mmo-cod-mod          .
           move      zero                 to   w-mmo-amc-prf
                                               w-mmo-ali-prf          .
           move      spaces               to   w-mmo-des-mod          .
       nor-mmo-999.
           exit.

      *    *===========================================================*
      *    * Start                                                     *
      *    *-----------------------------------------------------------*
       str-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           if        j-tre                =    "TST "
                     move  "tst "         to   pss-tre
           else if   j-tre                =    "STP "
                     move  "stp "         to   pss-tre
           else if   j-tre                =    "MOD "
                     move  "mod "         to   pss-tre
           else if   j-tre                =    "PMS "
                     move  "pms "         to   pss-tre
           else if   j-tre                =    "IMP "
                     move  "imp "         to   pss-tre
           else if   j-tre                =    "MMO "
                     move  "mmo "         to   pss-tre
           else      move  spaces         to   pss-tre                .
           move      j-kre                to   pss-kre                .
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           start     pss    key not less
                            pss-key
                            invalid key
                            go to   str-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to str-rec-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     str-rec-999.
       str-rec-400.
      *              *-------------------------------------------------*
      *              * Se start errata                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      "Start errata su [pss] !"
                                          to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
       str-rec-999.
           exit.

      *    *===========================================================*
      *    * Read next                                                 *
      *    *-----------------------------------------------------------*
       nxt-rec-000.
      *              *-------------------------------------------------*
      *              * Read next                                       *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss  next with no lock
                               at end
                               go to nxt-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to nxt-rec-600.
           if        e-sts                =    e-use-err
                     go to nxt-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     nxt-rec-999.
       nxt-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     nxt-rec-000.
       nxt-rec-400.
      *              *-------------------------------------------------*
      *              * Se at end                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-end-fil            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
       nxt-rec-490.
           move      "Fine file [pss] !"  to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     nxt-rec-999.
       nxt-rec-600.
      *              *-------------------------------------------------*
      *              * Se non at end                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se fine logica del file                *
      *                  *---------------------------------------------*
           if       (pss-tre              =    "tst" and
                     j-tre                =    "TST"   ) or
                    (pss-tre              =    "stp" and
                     j-tre                =    "STP"   ) or
                    (pss-tre              =    "mod" and
                     j-tre                =    "MOD"   ) or
                    (pss-tre              =    "pms" and
                     j-tre                =    "PMS"   ) or
                    (pss-tre              =    "imp" and
                     j-tre                =    "IMP"   ) or
                    (pss-tre              =    "mmo" and
                     j-tre                =    "MMO"   )
                     go to nxt-rec-800
           else      go to nxt-rec-400.
       nxt-rec-800.
      *                  *---------------------------------------------*
      *                  * Spostamento area dati in area di link       *
      *                  *---------------------------------------------*
           move      pss-kre              to   j-kre                  .
           move      pss-dat              to   j-dat                  .
       nxt-rec-999.
           exit.

      *    *===========================================================*
      *    * Read record                                               *
      *    *-----------------------------------------------------------*
       rea-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           if        j-tre                =    "TST "
                     move  "tst "         to   pss-tre
           else if   j-tre                =    "STP "
                     move  "stp "         to   pss-tre
           else if   j-tre                =    "MOD "
                     move  "mod "         to   pss-tre
           else if   j-tre                =    "PMS "
                     move  "pms "         to   pss-tre
           else if   j-tre                =    "IMP "
                     move  "imp "         to   pss-tre
           else if   j-tre                =    "MMO "
                     move  "mmo "         to   pss-tre
           else      move  spaces         to   pss-tre                .
           move      j-kre                to   pss-kre                .
      *              *-------------------------------------------------*
      *              * Lettura record senza lock                       *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    with no lock
                            invalid key
                            go to   rea-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to rea-rec-600.
           if        e-sts                =    e-use-err
                     go to rea-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-rec-999.
       rea-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     rea-rec-000.
       rea-rec-400.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-msg                  .
           if        j-tre                =    "TST "
                     go to rea-rec-410
           else if   j-tre                =    "STP "
                     go to rea-rec-420
           else if   j-tre                =    "MOD "
                     go to rea-rec-430
           else if   j-tre                =    "PMS "
                     go to rea-rec-440
           else if   j-tre                =    "IMP "
                     go to rea-rec-450
           else if   j-tre                =    "MMO "
                     go to rea-rec-460
           else      go to rea-rec-490.
       rea-rec-410.
           string    "Tipo stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-420.
           string    "Stampante codice '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-430.
           string    "Modulo codice '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-440.
           string    "Record parametri stampa per fase '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-450.
           string    "Impegno per '"
                                delimited by   size
                     j-kre
                                delimited by   size
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-460.
           string    "Record di modulo montato su stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-490.
           move      "Record non esistente !"
                                          to   w-msg                  .
       rea-rec-500.
           move      w-msg                to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area dati in work           *
      *                  *---------------------------------------------*
           perform   nor-rec-000          thru nor-rec-999            .
      *                  *---------------------------------------------*
      *                  * Ricostruzione chiave e dati                 *
      *                  *---------------------------------------------*
           if        j-tre                =    "TST "
                     move  j-kre          to   w-tst-cod-tst
                     move  w-tst          to   j-dat
           else if   j-tre                =    "STP "
                     move  j-kre          to   w-stp-cod-stp
                     move  w-stp          to   j-dat
           else if   j-tre                =    "MOD "
                     move  j-kre          to   w-mod-cod-mod
                     move  w-mod          to   j-dat
           else if   j-tre                =    "PMS "
                     move  j-kre          to   w-pms-fas-ges
                     move  w-pms          to   j-dat
           else if   j-tre                =    "IMP "
                     move  j-kre          to   w-imp-can-stp
                     move  w-imp          to   j-dat
           else if   j-tre                =    "MMO "
                     move  j-kre          to   w-mmo-cod-stp
                     move  w-mmo          to   j-dat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-rec-999.
       rea-rec-600.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spostamento area dati in area di link       *
      *                  *---------------------------------------------*
           move      pss-dat              to   j-dat                  .
       rea-rec-999.
           exit.

      *    *===========================================================*
      *    * Put record                                                *
      *    *-----------------------------------------------------------*
       put-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati                      *
      *              *-------------------------------------------------*
           if        j-tre                =    "TST "
                     move  "tst "         to   pss-tre
           else if   j-tre                =    "STP "
                     move  "stp "         to   pss-tre
           else if   j-tre                =    "MOD "
                     move  "mod "         to   pss-tre
           else if   j-tre                =    "PMS "
                     move  "pms "         to   pss-tre
           else if   j-tre                =    "IMP "
                     move  "imp "         to   pss-tre
           else if   j-tre                =    "MMO "
                     move  "mmo "         to   pss-tre
           else      move  spaces         to   pss-tre                .
           move      j-kre                to   pss-kre                .
           move      j-dat                to   pss-dat                .
      *              *-------------------------------------------------*
      *              * Scrittura record                                *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     pss-rec invalid key
                             go to   put-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in scrittura record                   *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to put-rec-999.
           if        e-sts                =    e-use-err
                     go to put-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-rec-999.
       put-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     put-rec-000.
       put-rec-400.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-dup-key            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-msg                  .
           if        j-tre                =    "TST "
                     go to put-rec-410
           else if   j-tre                =    "STP "
                     go to put-rec-420
           else if   j-tre                =    "MOD "
                     go to put-rec-430
           else if   j-tre                =    "PMS "
                     go to put-rec-440
           else if   j-tre                =    "IMP "
                     go to put-rec-450
           else if   j-tre                =    "MMO "
                     go to put-rec-460
           else      go to put-rec-490.
       put-rec-410.
           string    "Tipo stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-420.
           string    "Stampante codice '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-430.
           string    "Modulo codice '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-440.
           string    "Record parametri stampa per fase '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-450.
           string    "Impegno per '"
                                delimited by   size
                     j-kre
                                delimited by   size
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-460.
           string    "Record di modulo montato su stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-490.
           move      "Record gia' esistente !"
                                          to   w-msg                  .
       put-rec-500.
           move      w-msg                to   j-msg                  .
       put-rec-999.
           exit.

      *    *===========================================================*
      *    * Update record                                             *
      *    *-----------------------------------------------------------*
       upd-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati                      *
      *              *-------------------------------------------------*
           if        j-tre                =    "TST "
                     move  "tst "         to   pss-tre
           else if   j-tre                =    "STP "
                     move  "stp "         to   pss-tre
           else if   j-tre                =    "MOD "
                     move  "mod "         to   pss-tre
           else if   j-tre                =    "PMS "
                     move  "pms "         to   pss-tre
           else if   j-tre                =    "IMP "
                     move  "imp "         to   pss-tre
           else if   j-tre                =    "MMO "
                     move  "mmo "         to   pss-tre
           else      move  spaces         to   pss-tre                .
           move      j-kre                to   pss-kre                .
           move      j-dat                to   pss-dat                .
      *              *-------------------------------------------------*
      *              * Riscrittura record                              *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   pss-rec invalid key
                             go to   upd-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in riscritture record                 *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to upd-rec-999.
           if        e-sts                =    e-use-err
                     go to upd-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     upd-rec-999.
       upd-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     upd-rec-000.
       upd-rec-400.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-msg                  .
           if        j-tre                =    "TST "
                     go to upd-rec-410
           else if   j-tre                =    "STP "
                     go to upd-rec-420
           else if   j-tre                =    "MOD "
                     go to upd-rec-430
           else if   j-tre                =    "PMS "
                     go to upd-rec-440
           else if   j-tre                =    "IMP "
                     go to upd-rec-450
           else if   j-tre                =    "MMO "
                     go to upd-rec-460
           else      go to upd-rec-490.
       upd-rec-410.
           string    "Tipo stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-420.
           string    "Stampante codice '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-430.
           string    "Modulo codice '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-440.
           string    "Record parametri stampa per fase '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-450.
           string    "Impegno per '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-460.
           string    "Record di modulo montato su stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-490.
           move      "Record non esistente !"
                                          to   w-msg                  .
       upd-rec-500.
           move      w-msg                to   j-msg                  .
       upd-rec-999.
           exit.

      *    *===========================================================*
      *    * Delete record                                             *
      *    *-----------------------------------------------------------*
       del-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati                      *
      *              *-------------------------------------------------*
           if        j-tre                =    "TST "
                     move  "tst "         to   pss-tre
           else if   j-tre                =    "STP "
                     move  "stp "         to   pss-tre
           else if   j-tre                =    "MOD "
                     move  "mod "         to   pss-tre
           else if   j-tre                =    "PMS "
                     move  "pms "         to   pss-tre
           else if   j-tre                =    "IMP "
                     move  "imp "         to   pss-tre
           else if   j-tre                =    "MMO "
                     move  "mmo "         to   pss-tre
           else      move  spaces         to   pss-tre                .
           move      j-kre                to   pss-kre                .
           move      j-dat                to   pss-dat                .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           delete    pss     invalid key
                             go to   del-rec-999.
      *              *-------------------------------------------------*
      *              * Se errori in delete record                      *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to del-rec-999.
           if        e-sts                =    e-use-err
                     go to del-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     del-rec-999.
       del-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     del-rec-000.
       del-rec-999.
           exit.

      *    *===========================================================*
      *    * Preimpegno stampante                                      *
      *    *-----------------------------------------------------------*
       pim-stp-000.
      *              *-------------------------------------------------*
      *              * Se il canale di uscita indica che si tratta di  *
      *              * una stampante locale, o di una stampante di ti- *
      *              * po spool, o di tipo remote copy : uscita senza  *
      *              * alcuna azione                                   *
      *              *-------------------------------------------------*
           if        j-kre (01 : 01)      =    "#"     or
                     j-kre (01 : 05)      =    "spool" or
                     j-kre (01 : 03)      =    "rcp"   or
                     j-kre (01 : 03)      =    "pdf"
                     go to pim-stp-999.
       pim-stp-050.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      "imp "               to   pss-tre                .
           move      j-kre                to   pss-kre                .
      *              *-------------------------------------------------*
      *              * Lettura record con lock                         *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    invalid key
                            go to   pim-stp-200.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to pim-stp-400.
           if        e-sts                =    e-use-err
                     go to pim-stp-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pim-stp-999.
       pim-stp-100.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     pim-stp-050.
       pim-stp-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record preimpegno stampante    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Canale di uscita                        *
      *                      *-----------------------------------------*
           move      j-kre                to   w-imp-can-stp          .
      *                      *-----------------------------------------*
      *                      * Status di impegno della stampante       *
      *                      *-----------------------------------------*
           move      01                   to   w-imp-sts-imp          .
      *                      *-----------------------------------------*
      *                      * Azienda                                 *
      *                      *-----------------------------------------*
           move      r-env-cod-azi        to   w-imp-azi-imp          .
      *                      *-----------------------------------------*
      *                      * Terminale                               *
      *                      *-----------------------------------------*
           move      r-env-cod-ter        to   w-imp-ter-imp          .
      *                      *-----------------------------------------*
      *                      * Utente                                  *
      *                      *-----------------------------------------*
           move      r-env-cod-ute        to   w-imp-ute-imp          .
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      r-ide-sis-app        to   w-imp-sap-imp          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      r-ide-are-ges        to   w-imp-arg-imp          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      r-ide-set-ges        to   w-imp-set-imp          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      r-ide-fas-ges        to   w-imp-fas-imp          .
      *                      *-----------------------------------------*
      *                      * System date and time                    *
      *                      *-----------------------------------------*
           move      r-env-dat-tim        to   w-imp-sdt-imp          .
      *                      *-----------------------------------------*
      *                      * Scrittura record per preimpegno         *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      w-imp                to   pss-dat                .
           write     pss-rec
                         invalid key
                         go to   pim-stp-050.
      *                      *-----------------------------------------*
      *                      * Se errori in scrittura record           *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to pim-stp-300.
      *                          *-------------------------------------*
      *                          * Visualizzazione messaggio di errore *
      *                          *-------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     pim-stp-999.
       pim-stp-300.
      *                      *-----------------------------------------*
      *                      * Se record scritto correttamente         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Record preimpegnato in area di link *
      *                          *-------------------------------------*
           move      w-imp                to   j-dat                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     pim-stp-999.
       pim-stp-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spostamento area dati in area di work       *
      *                  *---------------------------------------------*
           move      pss-dat              to   w-imp                  .
      *                  *---------------------------------------------*
      *                  * Test su tipo impegno attuale                *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    02
                     go to pim-stp-450
           else if   w-imp-sts-imp        =    03
                     go to pim-stp-500
           else if   w-imp-sts-imp        =    04
                     go to pim-stp-550
           else if   w-imp-sts-imp        =    01
                     go to pim-stp-600
           else if   w-imp-sts-imp        =    05
                     go to pim-stp-650
           else      go to pim-stp-700.
       pim-stp-450.
      *                  *---------------------------------------------*
      *                  * Tipo impegno : manutenzione stampante  "02" *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Return status code : non ottenuto       *
      *                      *-----------------------------------------*
           move      e-use-err            to   j-rsc                  .
      *                      *-----------------------------------------*
      *                      * Return message                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' impegnata in operazioni di manutenzione."
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *                      *-----------------------------------------*
      *                      * Spostamento area dati in area di link   *
      *                      *-----------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                      *-----------------------------------------*
      *                      * Unlock records                          *
      *                      *-----------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pim-stp-999.
       pim-stp-500.
      *                  *---------------------------------------------*
      *                  * Tipo impegno : montaggio modulo        "03" *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Return status code : non ottenuto       *
      *                      *-----------------------------------------*
           move      e-use-err            to   j-rsc                  .
      *                      *-----------------------------------------*
      *                      * Return message                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' impegnata in operazioni di montaggio modulo."
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *                      *-----------------------------------------*
      *                      * Spostamento area dati in area di link   *
      *                      *-----------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                      *-----------------------------------------*
      *                      * Unlock records                          *
      *                      *-----------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pim-stp-999.
       pim-stp-550.
      *                  *---------------------------------------------*
      *                  * Tipo impegno : servizio di spool       "04" *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Return status code : non ottenuto       *
      *                      *-----------------------------------------*
           move      e-use-err            to   j-rsc                  .
      *                      *-----------------------------------------*
      *                      * Return message                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' impegnata per servizio di spool."
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *                      *-----------------------------------------*
      *                      * Spostamento area dati in area di link   *
      *                      *-----------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                      *-----------------------------------------*
      *                      * Unlock records                          *
      *                      *-----------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pim-stp-999.
       pim-stp-600.
      *                  *---------------------------------------------*
      *                  * Tipo impegno : preimpegno              "01" *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su chi effettua il preimpegno      *
      *                      *-----------------------------------------*
           if        w-imp-azi-imp        =    r-env-cod-azi and
                     w-imp-ute-imp        =    r-env-cod-ute and
                     w-imp-ter-imp        =    r-env-cod-ter and
                     w-imp-fas-imp        =    r-ide-fas-ges
                     go to pim-stp-610
           else      go to pim-stp-620.
       pim-stp-610.
      *                      *-----------------------------------------*
      *                      * Se il preimpegno e' in atto da parte    *
      *                      * dello stesso utente, allo stesso ter-   *
      *                      * minale, e per la stessa fase gestionale *
      *                      * per la stessa azienda                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Spostamento area dati in link area  *
      *                          *-------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                          *-------------------------------------*
      *                          * Unlock records                      *
      *                          *-------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     pim-stp-999.
       pim-stp-620.
      *                      *-----------------------------------------*
      *                      * Se il preimpegno e' in atto da parte    *
      *                      * di un altro utente, o ad un altro ter-  *
      *                      * minale, o per un'altra fase gestionale  *
      *                      * o per un'altra azienda                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Return status code : non ottenuto   *
      *                          *-------------------------------------*
           move      e-use-err            to   j-rsc                  .
      *                          *-------------------------------------*
      *                          * Return message                      *
      *                          *-------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Richiesta di impegno per stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' in atto."
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *                          *-------------------------------------*
      *                          * Spostamento area dati in link area  *
      *                          *-------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                          *-------------------------------------*
      *                          * Unlock records                      *
      *                          *-------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     pim-stp-999.
       pim-stp-650.
      *                  *---------------------------------------------*
      *                  * Tipo impegno : in fase di stampa       "05" *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su chi effettua l'impegno          *
      *                      *-----------------------------------------*
           if        w-imp-azi-imp        =    r-env-cod-azi and
                     w-imp-ute-imp        =    r-env-cod-ute and
                     w-imp-ter-imp        =    r-env-cod-ter and
                     w-imp-fas-imp        =    r-ide-fas-ges
                     go to pim-stp-660
           else      go to pim-stp-680.
       pim-stp-660.
      *                      *-----------------------------------------*
      *                      * Se l'impegno e' in atto da parte dello  *
      *                      * stesso utente, allo stesso terminale,   *
      *                      * per la stessa fase gestionale, e per    *
      *                      * la stessa azienda                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Composizione record preimpegno      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Canale di uscita                *
      *                              *---------------------------------*
           move      j-kre                to   w-imp-can-stp          .
      *                              *---------------------------------*
      *                              * Status di impegno               *
      *                              *---------------------------------*
           move      01                   to   w-imp-sts-imp          .
      *                              *---------------------------------*
      *                              * Codice azienda                  *
      *                              *---------------------------------*
           move      r-env-cod-azi        to   w-imp-azi-imp          .
      *                              *---------------------------------*
      *                              * Codice terminale                *
      *                              *---------------------------------*
           move      r-env-cod-ter        to   w-imp-azi-imp          .
      *                              *---------------------------------*
      *                              * Codice utente                   *
      *                              *---------------------------------*
           move      r-env-cod-ute        to   w-imp-azi-imp          .
      *                              *---------------------------------*
      *                              * Sistema applicativo             *
      *                              *---------------------------------*
           move      r-ide-sis-app        to   w-imp-sap-imp          .
      *                              *---------------------------------*
      *                              * Area gestionale                 *
      *                              *---------------------------------*
           move      r-ide-are-ges        to   w-imp-arg-imp          .
      *                              *---------------------------------*
      *                              * Settore gestionale              *
      *                              *---------------------------------*
           move      r-ide-set-ges        to   w-imp-set-imp          .
      *                              *---------------------------------*
      *                              * Fase gestionale                 *
      *                              *---------------------------------*
           move      r-ide-fas-ges        to   w-imp-fas-imp          .
      *                              *---------------------------------*
      *                              * System date and time            *
      *                              *---------------------------------*
           move      r-env-dat-tim        to   w-imp-sdt-imp          .
      *                              *---------------------------------*
      *                              * Riscrittura record preimpegno   *
      *                              *---------------------------------*
           move      e-not-err            to   e-sts                  .
           move      w-imp                to   pss-dat                .
           rewrite   pss-rec
                         invalid key
                         go to   pim-stp-050.
      *                              *---------------------------------*
      *                              * Se errori in riscrittura record *
      *                              *---------------------------------*
           if        e-sts                =    e-not-err
                     go to pim-stp-670.
      *                                  *-----------------------------*
      *                                  * Visualizzazione messaggio   *
      *                                  * di errore                   *
      *                                  *-----------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     pim-stp-999.
       pim-stp-670.
      *                          *-------------------------------------*
      *                          * Spostamento area dati in link area  *
      *                          *-------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                          *-------------------------------------*
      *                          * Unlock records                      *
      *                          *-------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     pim-stp-999.
       pim-stp-680.
      *                      *-----------------------------------------*
      *                      * Se l'impegno e' in atto da parte di un  *
      *                      * altro utente, o ad un altro terminale,  *
      *                      * o per un'altra fase gestionale, o per   *
      *                      * un'altra azienda                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Return status code : non ottenuto   *
      *                          *-------------------------------------*
           move      e-use-err            to   j-rsc                  .
      *                          *-------------------------------------*
      *                          * Return message                      *
      *                          *-------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' impegnata in fase di stampa."
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *                          *-------------------------------------*
      *                          * Spostamento area dati in link area  *
      *                          *-------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                          *-------------------------------------*
      *                          * Unlock records                      *
      *                          *-------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     pim-stp-999.
       pim-stp-700.
      *                  *---------------------------------------------*
      *                  * Tipo impegno : non impegnata                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione record preimpegno          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Canale di uscita                    *
      *                          *-------------------------------------*
           move      j-kre                to   w-imp-can-stp          .
      *                          *-------------------------------------*
      *                          * Status di impegno                   *
      *                          *-------------------------------------*
           move      01                   to   w-imp-sts-imp          .
      *                          *-------------------------------------*
      *                          * Codice azienda                      *
      *                          *-------------------------------------*
           move      r-env-cod-azi        to   w-imp-azi-imp          .
      *                          *-------------------------------------*
      *                          * Codice terminale                    *
      *                          *-------------------------------------*
           move      r-env-cod-ter        to   w-imp-ter-imp          .
      *                          *-------------------------------------*
      *                          * Codice utente                       *
      *                          *-------------------------------------*
           move      r-env-cod-ute        to   w-imp-ute-imp          .
      *                          *-------------------------------------*
      *                          * Sistema applicativo                 *
      *                          *-------------------------------------*
           move      r-ide-sis-app        to   w-imp-sap-imp          .
      *                          *-------------------------------------*
      *                          * Area gestionale                     *
      *                          *-------------------------------------*
           move      r-ide-are-ges        to   w-imp-arg-imp          .
      *                          *-------------------------------------*
      *                          * Settore gestionale                  *
      *                          *-------------------------------------*
           move      r-ide-set-ges        to   w-imp-set-imp          .
      *                          *-------------------------------------*
      *                          * Fase gestionale                     *
      *                          *-------------------------------------*
           move      r-ide-fas-ges        to   w-imp-fas-imp          .
      *                          *-------------------------------------*
      *                          * System date and time                *
      *                          *-------------------------------------*
           move      r-env-dat-tim        to   w-imp-sdt-imp          .
      *                          *-------------------------------------*
      *                          * Riscrittura record per preimpegno   *
      *                          *-------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      w-imp                to   pss-dat                .
           rewrite   pss-rec
                         invalid key
                         go to   pim-stp-050.
      *                          *-------------------------------------*
      *                          * Se errori in riscrittura record     *
      *                          *-------------------------------------*
           if        e-sts                =    e-not-err
                     go to pim-stp-710.
      *                              *---------------------------------*
      *                              * Visualizzazione messaggio di    *
      *                              * errore a centro video e chiu-   *
      *                              * sura modulo                     *
      *                              *---------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     pim-stp-999.
       pim-stp-710.
      *                          *-------------------------------------*
      *                          * Spostamento area dati in link area  *
      *                          *-------------------------------------*
           move      pss-dat              to   j-dat                  .
      *                          *-------------------------------------*
      *                          * Unlock records                      *
      *                          *-------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
       pim-stp-999.
           exit.

      *    *===========================================================*
      *    * Impegno stampante                                         *
      *    *-----------------------------------------------------------*
       imp-stp-000.
      *              *-------------------------------------------------*
      *              * Se il canale di uscita indica che si tratta di  *
      *              * una stampante locale, o di una stampante di ti- *
      *              * po spool, o di tipo remote copy : uscita senza  *
      *              * alcuna azione                                   *
      *              *-------------------------------------------------*
           if        j-kre (01 : 01)      =    "#"     or
                     j-kre (01 : 05)      =    "spool" or
                     j-kre (01 : 03)      =    "rcp"   or
                     j-kre (01 : 03)      =    "pdf"
                     go to imp-stp-999.
       imp-stp-050.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      "imp "               to   pss-tre                .
           move      j-kre                to   pss-kre                .
      *              *-------------------------------------------------*
      *              * Lettura record con lock                         *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    invalid key
                            go to   imp-stp-200.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to imp-stp-300.
           if        e-sts                =    e-use-err
                     go to imp-stp-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     imp-stp-999.
       imp-stp-100.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     imp-stp-050.
       imp-stp-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione area dati in work              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Canale di uscita                        *
      *                      *-----------------------------------------*
           move      j-kre                to   w-imp-can-stp          .
      *                      *-----------------------------------------*
      *                      * Status di impegno                       *
      *                      *-----------------------------------------*
           if        j-ope                =    "01"
                     move  01             to   w-imp-sts-imp
           else if   j-ope                =    "02"
                     move  02             to   w-imp-sts-imp
           else if   j-ope                =    "03"
                     move  03             to   w-imp-sts-imp
           else if   j-ope                =    "04"
                     move  04             to   w-imp-sts-imp
           else if   j-ope                =    "05"
                     move  05             to   w-imp-sts-imp
           else      move  00             to   w-imp-sts-imp          .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-env-cod-azi  to   w-imp-azi-imp
           else      move  spaces         to   w-imp-azi-imp          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    02 or
                     w-imp-sts-imp        =    03 or
                     w-imp-sts-imp        =    05
                     move  r-env-cod-ter  to   w-imp-ter-imp
           else      move  spaces         to   w-imp-ter-imp          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    02 or
                     w-imp-sts-imp        =    03 or
                     w-imp-sts-imp        =    05
                     move  r-env-cod-ute  to   w-imp-ute-imp
           else      move  spaces         to   w-imp-ute-imp          .
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-sis-app  to   w-imp-sap-imp
           else      move  spaces         to   w-imp-sap-imp          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-are-ges  to   w-imp-arg-imp
           else      move  spaces         to   w-imp-arg-imp          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-set-ges  to   w-imp-set-imp
           else      move  spaces         to   w-imp-set-imp          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-fas-ges  to   w-imp-fas-imp
           else      move  spaces         to   w-imp-fas-imp          .
      *                      *-----------------------------------------*
      *                      * System date and time                    *
      *                      *-----------------------------------------*
           move      r-env-dat-tim        to   w-imp-sdt-imp          .
      *                  *---------------------------------------------*
      *                  * Scrittura record impegno                    *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      w-imp                to   pss-dat                .
           write     pss-rec
                         invalid key
                         go to   imp-stp-050.
      *                  *---------------------------------------------*
      *                  * Se errori in scrittura record               *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to imp-stp-500.
      *                      *-----------------------------------------*
      *                      * Visualizzazione messaggio di errore     *
      *                      *-----------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     imp-stp-999.
       imp-stp-300.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione area dati in work              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Canale di uscita                        *
      *                      *-----------------------------------------*
           move      j-kre                to   w-imp-can-stp          .
      *                      *-----------------------------------------*
      *                      * Status di impegno                       *
      *                      *-----------------------------------------*
           if        j-ope                =    "01"
                     move  01             to   w-imp-sts-imp
           else if   j-ope                =    "02"
                     move  02             to   w-imp-sts-imp
           else if   j-ope                =    "03"
                     move  03             to   w-imp-sts-imp
           else if   j-ope                =    "04"
                     move  04             to   w-imp-sts-imp
           else if   j-ope                =    "05"
                     move  05             to   w-imp-sts-imp
           else      move  00             to   w-imp-sts-imp          .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-env-cod-azi  to   w-imp-azi-imp
           else      move  spaces         to   w-imp-azi-imp          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    02 or
                     w-imp-sts-imp        =    03 or
                     w-imp-sts-imp        =    05
                     move  r-env-cod-ter  to   w-imp-ter-imp
           else      move  spaces         to   w-imp-ter-imp          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    02 or
                     w-imp-sts-imp        =    03 or
                     w-imp-sts-imp        =    05
                     move  r-env-cod-ute  to   w-imp-ute-imp
           else      move  spaces         to   w-imp-ute-imp          .
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-sis-app  to   w-imp-sap-imp
           else      move  spaces         to   w-imp-sap-imp          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-are-ges  to   w-imp-arg-imp
           else      move  spaces         to   w-imp-arg-imp          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-set-ges  to   w-imp-set-imp
           else      move  spaces         to   w-imp-set-imp          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        =    05
                     move  r-ide-fas-ges  to   w-imp-fas-imp
           else      move  spaces         to   w-imp-fas-imp          .
      *                      *-----------------------------------------*
      *                      * System date and time                    *
      *                      *-----------------------------------------*
           move      r-env-dat-tim        to   w-imp-sdt-imp          .
      *                  *---------------------------------------------*
      *                  * Riscrittura record impegno                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      w-imp                to   pss-dat                .
           rewrite   pss-rec
                         invalid key
                         go to   imp-stp-400.
      *                  *---------------------------------------------*
      *                  * Se errori in riscrittura record             *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to imp-stp-500.
      *                      *-----------------------------------------*
      *                      * Visualizzazione messaggio di errore     *
      *                      *-----------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     imp-stp-999.
       imp-stp-400.
      *                  *---------------------------------------------*
      *                  * Se record non esistente in riscrittura      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Return status code                      *
      *                      *-----------------------------------------*
           move      e-use-err            to   j-rsc                  .
      *                      *-----------------------------------------*
      *                      * Return message                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Impegno stampante '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non riuscito !"
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *                      *-----------------------------------------*
      *                      * Unlock records                          *
      *                      *-----------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     imp-stp-999.
       imp-stp-500.
      *                  *---------------------------------------------*
      *                  * Se impegno eseguito correttamente           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock records                          *
      *                      *-----------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
       imp-stp-999.
           exit.

      *    *===========================================================*
      *    * Rilascio impegno o preimpegno stampante                   *
      *    *-----------------------------------------------------------*
       rel-stp-000.
      *              *-------------------------------------------------*
      *              * Se il canale di uscita indica che si tratta di  *
      *              * una stampante locale, o di una stampante di ti- *
      *              * po spool, o di tipo remote copy : uscita senza  *
      *              * alcuna azione                                   *
      *              *-------------------------------------------------*
           if        j-kre (01 : 01)      =    "#"     or
                     j-kre (01 : 05)      =    "spool" or
                     j-kre (01 : 03)      =    "rcp"   or
                     j-kre (01 : 03)      =    "pdf"
                     go to rel-stp-999.
       rel-stp-050.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      "imp "               to   pss-tre                .
           move      j-kre                to   pss-kre                .
      *              *-------------------------------------------------*
      *              * Lettura record con lock                         *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    invalid key
                            go to   rel-stp-999.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to rel-stp-200.
           if        e-sts                =    e-use-err
                     go to rel-stp-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rel-stp-999.
       rel-stp-100.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     rel-stp-050.
       rel-stp-200.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione area dati in work           *
      *                  *---------------------------------------------*
           move      spaces               to   w-imp-can-stp          .
           move      zero                 to   w-imp-sts-imp          .
           move      spaces               to   w-imp-azi-imp
                                               w-imp-ter-imp
                                               w-imp-ute-imp
                                               w-imp-sap-imp
                                               w-imp-arg-imp
                                               w-imp-set-imp
                                               w-imp-fas-imp
           move      zero                 to   w-imp-sdt-imp          .
      *                  *---------------------------------------------*
      *                  * Riscrittura record preimpegno               *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      w-imp                to   pss-dat                .
           rewrite   pss-rec
                         invalid key
                         go to   rel-stp-300.
      *                  *---------------------------------------------*
      *                  * Se errori in riscrittura record             *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to rel-stp-300.
      *                      *-----------------------------------------*
      *                      * Visualizzazione messaggio di errore     *
      *                      *-----------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rel-stp-999.
       rel-stp-300.
      *                  *---------------------------------------------*
      *                  * Se record non esistente in riscrittura o se *
      *                  * riscrittura eseguita correttamente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock records                          *
      *                      *-----------------------------------------*
           perform   unl-pss-000          thru unl-pss-999            .
       rel-stp-999.
           exit.

      *    *===========================================================*
      *    * Unlock records file [pss]                                 *
      *    *-----------------------------------------------------------*
       unl-pss-000.
           unlock    pss   records                                    .
       unl-pss-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione del messaggio di errore a centro video e  *
      *    * chiusura del modulo                                       *
      *    *-----------------------------------------------------------*
       msg-err-000.
      *              *-------------------------------------------------*
      *              * Return status code                              *
      *              *-------------------------------------------------*
           move      "##"                 to   j-rsc                  .
      *              *-------------------------------------------------*
      *              * Return message                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Errore di i-o codice '"
                                delimited by   size
                     e-sts
                                delimited by   size
                     "' su file '"
                                delimited by   size
                     f-pss-pat
                                delimited by   spaces
                     "' !"
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *              *-------------------------------------------------*
      *              * Emissione messaggio ed accettazione OK          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-msg                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in on                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .



      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       msg-err-300.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to msg-err-400.
           if        v-alf                not  = "OK"
                     go to msg-err-300.
       msg-err-400.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-err-999.
           exit.

      *    *===========================================================*
      *    * Wait di un secondo                                        *
      *    *-----------------------------------------------------------*
       wai-t1s-000.
           call      "swd/mod/prg/obj/mwait0"                         .
       wai-t1s-999.
           exit.
