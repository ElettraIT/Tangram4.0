       Identification Division.
       Program-Id.                                 mbckgr             .
      *================================================================*
      *                                                                *
      *             Trattamento file richieste per backgrounds         *
      *                                                                *
      *================================================================*
      *                                                                *
      *             ================================================== *
      *             NOTA IMPORTANTE                                    *
      *             -------------------------------------------------- *
      *                                                                *
      *             Il 'background' e' stato definitivamente soppresso *
      *             e tutti i programmi funzionano in 'foreground'     *
      *                                                                *
      *             Il modulo viene comunque utilizzato per passare    *
      *             le 'richieste' tra i programmi di stampa           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Inizio scrittura record richieste .                *
      * Output                                                         *
      *             Input  : b-ope = "OO"                              *
      *                                                                *
      *                      b-tfe = Tipo di funzionamento del pro-    *
      *                              gramma di esecuzione              *
      *                              - F : Foreground                  *
      *                              - B : Background                  *
      *                                                                *
      *             Output : b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error verificatosi *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put         Scrittura segmento del record richieste .          *
      *                                                                *
      *             Input  : b-ope = "PT"                              *
      *                                                                *
      *                      b-chr = Segmento da 255  bytes del record *
      *                              richieste                         *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Inizio lettura record richieste .                  *
      * Input                                                          *
      *             Input  : b-ope = "OI"                              *
      *                                                                *
      *                      b-tfe = Tipo di funzionamento del pro-    *
      *                              gramma di esecuzione              *
      *                              - F : Foreground                  *
      *                              - B : Background                  *
      *                                                                *
      *             Output : b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error verificatosi *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Get         Lettura segmento del record richieste .            *
      *                                                                *
      *             Input  : b-ope = "GT"                              *
      *                                                                *
      *             Output : b-chr = Segmento da 255  bytes del record *
      *                              richieste                         *
      *                                                                *
      *                      b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - ##     : fine record            *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Fine scrittura o lettura del record richieste.     *
      *                                                                *
      *             Input  : b-ope = "CL"                              *
      *                                                                *
      *             Output : b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error verificatosi *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .
       
      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento del programma di esecuzione        *
      *        *-------------------------------------------------------*
           05  w-tfe                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tipo open in corso                            *
      *        *-------------------------------------------------------*
           05  w-top                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore per numero progressivo di segmento          *
      *        *-------------------------------------------------------*
           05  w-cns                      pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Puntatore per string-unstring su record richieste     *
      *        *-------------------------------------------------------*
           05  w-pnt                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per puntatore precedente                  *
      *        *-------------------------------------------------------*
           05  w-svp                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodo di 255  caratteri per string-unstring          *
      *        *-------------------------------------------------------*
           05  w-seu.
               10  filler occurs 255      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore della environment-variable 'V_DKB_PFIX'        *
      *        *-------------------------------------------------------*
           05  w-ppu                      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work-area per informazioni per messaggio di errore    *
      *        *-------------------------------------------------------*
           05  w-inf-err.
               10  w-inf-azi              pic  x(04)                  .
               10  w-inf-ter              pic  x(08)                  .
               10  w-inf-ute              pic  x(08)                  .
               10  w-inf-sap              pic  x(03)                  .
               10  w-inf-arg              pic  x(03)                  .
               10  w-inf-set              pic  x(03)                  .
               10  w-inf-fas              pic  x(06)                  .
               10  w-inf-pro              pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      ******************************************************************
       Procedure Division                using b                      .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Open output                                     *
      *              *-------------------------------------------------*
           if        b-ope                =    "OO"
                     perform opn-out-000  thru opn-out-999
      *              *-------------------------------------------------*
      *              * Open input                                      *
      *              *-------------------------------------------------*
           else if   b-ope                =    "OI"
                     perform opn-inp-000  thru opn-inp-999
      *              *-------------------------------------------------*
      *              * Put segmento                                    *
      *              *-------------------------------------------------*
           else if   b-ope                =    "PT"
                     perform put-sgm-000  thru put-sgm-999
      *              *-------------------------------------------------*
      *              * Get segmento                                    *
      *              *-------------------------------------------------*
           else if   b-ope                =    "GT"
                     perform get-sgm-000  thru get-sgm-999
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           else if   b-ope                =    "CL"
                     perform cls-cls-000  thru cls-cls-999            .
       main-999.
           exit      program.

      *    *===========================================================*
      *    * Open output                                               *
      *    *-----------------------------------------------------------*
       opn-out-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Flag di tipo open in corso                      *
      *              *-------------------------------------------------*
           move      "O"                  to   w-top                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo di funzionamento del pro-   *
      *              * gramma di esecuzione                            *
      *              *-------------------------------------------------*
           move      b-tfe                to   w-tfe                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore per numero progres-  *
      *              * sivo di segmento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-cns                  .
      *              *-------------------------------------------------*
      *              * Test se funzionamento foreground o background   *
      *              *-------------------------------------------------*
           if        w-tfe                not  = "B"
                     go to opn-out-800.
      *                  *---------------------------------------------*
      *                  * Se background                               *
      *                  *---------------------------------------------*
      *                  * NON PIU' UTILIZZATO !                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prefisso o postfisso unico per files    *
      *                      * temporanei da segreteria                *
      *                      *-----------------------------------------*
           move      "U?"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-ppu                  .
      *                      *-----------------------------------------*
      *                      * Preparazione name file richieste        *
      *                      *-----------------------------------------*
           move      "ric "               to   g-nam                  .
      *                      *-----------------------------------------*
      *                      * Preparazione pathname file richieste    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    w-ppu      delimited by   spaces
                     ".ric"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   g-pat                  .
      *                      *-----------------------------------------*
      *                      * Open output                             *
      *                      *-----------------------------------------*
           move      "OO"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Test se errori                          *
      *                      *-----------------------------------------*
           if        g-sts                =    e-not-err
                     go to opn-out-800.
      *                      *-----------------------------------------*
      *                      * Se errori                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si ritorna lo status di errore      *
      *                          *-------------------------------------*
           move      g-sts                to   b-rsc                  .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [ric]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Emissione messaggio d'errore        *
      *                          *-------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     opn-out-999.
       opn-out-800.
      *                  *---------------------------------------------*
      *                  * Se foreground                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione record richieste       *
      *                      *-----------------------------------------*
           move      spaces               to   g-rec                  .
       opn-out-999.
           exit.

      *    *===========================================================*
      *    * Put segmento di record richieste                          *
      *    *-----------------------------------------------------------*
       put-sgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in record richieste *
      *              *-------------------------------------------------*
           move      w-cns                to   w-pnt                  .
           multiply  255                  by   w-pnt                  .
           add       1                    to   w-pnt                  .
           add       1                    to   w-cns                  .
           move      b-chr                to   w-seu                  .
           string    w-seu
                     delimited by size    into g-rec
                                  with pointer w-pnt                  .
       put-sgm-999.
           exit.

      *    *===========================================================*
      *    * Open input                                                *
      *    *-----------------------------------------------------------*
       opn-inp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Flag di tipo open in corso                      *
      *              *-------------------------------------------------*
           move      "I"                  to   w-top                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo di funzionamento del pro-   *
      *              * gramma di esecuzione                            *
      *              *-------------------------------------------------*
           move      b-tfe                to   w-tfe                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore per numero progres-  *
      *              * sivo di segmento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-cns                  .
      *              *-------------------------------------------------*
      *              * Test se funzionamento foreground o background   *
      *              *-------------------------------------------------*
           if        w-tfe                not  = "B"
                     go to opn-inp-999.
      *                  *---------------------------------------------*
      *                  * Se background                               *
      *                  *---------------------------------------------*
      *                  * NON PIU' UTILIZZATO !                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prefisso o postfisso unico per files    *
      *                      * temporanei da segreteria                *
      *                      *-----------------------------------------*
           move      "U?"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-ppu                  .
      *                      *-----------------------------------------*
      *                      * Preparazione name file richieste        *
      *                      *-----------------------------------------*
           move      "ric "               to   g-nam                  .
      *                      *-----------------------------------------*
      *                      * Preparazione pathname file richieste    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    w-ppu      delimited by   spaces
                     ".ric"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   g-pat                  .
      *                      *-----------------------------------------*
      *                      * Open input                              *
      *                      *-----------------------------------------*
           move      "OI"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Test se errori                          *
      *                      *-----------------------------------------*
           if        g-sts                =    e-not-err
                     go to opn-inp-200.
      *                      *-----------------------------------------*
      *                      * Se errori                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si ritorna lo status di errore      *
      *                          *-------------------------------------*
           move      g-sts                to   b-rsc                  .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [ric]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Emissione messaggio d'errore        *
      *                          *-------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     opn-inp-999.
       opn-inp-200.
      *                      *-----------------------------------------*
      *                      * Normalizzazione area da leggere         *
      *                      *-----------------------------------------*
           move      spaces               to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Lettura del record richieste            *
      *                      *-----------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Test su errori                          *
      *                      *-----------------------------------------*
           if        g-sts                =    e-not-err
                     go to opn-inp-999.
      *                      *-----------------------------------------*
      *                      * Se errori                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si ritorna lo status di errore      *
      *                          *-------------------------------------*
           move      g-sts                to   b-rsc                  .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [ric]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Emissione messaggio d'errore        *
      *                          *-------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     opn-inp-999.
       opn-inp-999.
           exit.

      *    *===========================================================*
      *    * Get segmento di record richieste                          *
      *    *-----------------------------------------------------------*
       get-sgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Estrazione del segmento dal record richieste    *
      *              *-------------------------------------------------*
           move      spaces               to   w-seu                  .
           move      w-cns                to   w-pnt                  .
           multiply  255                  by   w-pnt                  .
           add       1                    to   w-pnt                  .
           add       1                    to   w-cns                  .
           move      w-pnt                to   w-svp                  .
           unstring  g-rec                into w-seu
                                  with pointer w-pnt                  .
           move      w-seu                to   b-chr                  .
           if        w-pnt                =    w-svp
                     move  "##"           to   b-rsc
           else      move  spaces         to   b-rsc                  .
       get-sgm-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-cls-000.
      *              *-------------------------------------------------*
      *              * Se in corso open output                         *
      *              *-------------------------------------------------*
           if        w-top                =    "O"
                     perform cls-out-000  thru cls-out-999
      *              *-------------------------------------------------*
      *              * Se in corso open input                          *
      *              *-------------------------------------------------*
           else      perform cls-inp-000  thru cls-inp-999            .
       cls-cls-999.
           exit.

      *    *===========================================================*
      *    * Close output                                              *
      *    *-----------------------------------------------------------*
       cls-out-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Test se funzionamento foreground o background   *
      *              *-------------------------------------------------*
           if        w-tfe                not  = "B"
                     go to cls-out-999.
       cls-out-200.
      *              *-------------------------------------------------*
      *              * Se background                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to cls-out-300.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                          *-------------------------------------*
      *                          * Si ritorna lo status di errore      *
      *                          *-------------------------------------*
           move      g-sts                to   b-rsc                  .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [ric]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Emissione messaggio d'errore        *
      *                          *-------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     cls-out-999.
       cls-out-300.
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to cls-out-999.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si ritorna lo status di errore          *
      *                      *-----------------------------------------*
           move      g-sts                to   b-rsc                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio d'errore            *
      *                      *-----------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cls-out-999.
       cls-out-999.
           exit.

      *    *===========================================================*
      *    * Close input                                               *
      *    *-----------------------------------------------------------*
       cls-inp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Test se funzionamento foreground o background   *
      *              *-------------------------------------------------*
           if        w-tfe                not  = "B"
                     go to cls-inp-999.
       cls-inp-200.
      *              *-------------------------------------------------*
      *              * Se background                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to cls-inp-999.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si ritorna lo status di errore          *
      *                      *-----------------------------------------*
           move      g-sts                to   b-rsc                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio d'errore            *
      *                      *-----------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cls-inp-999.
       cls-inp-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di errore                             *
      *    *-----------------------------------------------------------*
       out-err-000.
      *              *-------------------------------------------------*
      *              * Raccolta informazioni da segreteria             *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   w-inf-azi              .
           move      s-ter                to   w-inf-ter              .
           move      s-ute                to   w-inf-ute              .
           move      s-sap                to   w-inf-sap              .
           move      s-arg                to   w-inf-arg              .
           move      s-set                to   w-inf-set              .
           move      s-fas                to   w-inf-fas              .
           move      s-pro                to   w-inf-pro              .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di emissione messaggio di   *
      *              * errore                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
           move      g-nam                to   s-nam                  .
           move      g-pat                to   s-pat                  .
           move      b-rsc                to   s-sts                  .
           move      w-tfe                to   s-fun                  .
           move      w-inf-azi            to   s-azi                  .
           move      w-inf-ter            to   s-ter                  .
           move      w-inf-ute            to   s-ute                  .
           move      w-inf-sap            to   s-sap                  .
           move      w-inf-arg            to   s-arg                  .
           move      w-inf-set            to   s-set                  .
           move      w-inf-fas            to   s-fas                  .
           move      w-inf-pro            to   s-pro                  .
      *                  *---------------------------------------------*
      *                  * Richiamo modulo                             *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mioerr"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mioerr"                         .
       out-err-999.
           exit.
