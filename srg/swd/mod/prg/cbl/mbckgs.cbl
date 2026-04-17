       Identification Division.
       Program-Id.                                 mbckgs             .
      *================================================================*
      *                                                                *
      *             Trattamento file segreteria per backgrounds        *
      *                                                                *
      *             NOTA NdeK   : sono state inibite le istruzioni     *
      *                           RM Cobol per evitare problemi di     *
      *                           compilazione                         *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *     ---------------------------------------------------------- *
      *                                                                *
      * Nota importante : Questo modulo NON utilizza per il trattamen- *
      *                   to del line sequential file l'apposito modu- *
      *                   lo "mlsfil".                                 *
      *                   Questo per poter trattare un record piu'     *
      *                   lungo di quanto gestito da "mlsfil".         *
      *                                                                *
      *                   Cio' e' possibile perche' non e' tassativo   *
      *                   che venga prodotto un record compatibile     *
      *                   con lo standard editor 'vi', essendo l'uti-  *
      *                   lizzo del record immagine di segreteria del  *
      *                   tutto locale e temporaneo.                   *
      *                                                                *
      * Nota importante : Vengono utilizzati due files distinti a se-  *
      *                   conda che il Runtime utilizzato sia quello   *
      *                   Austec oppure un altro.                      *
      *                   Questo perche' per il Runtime Austec e' ne-  *
      *                   cessario eseguire una 'write before 1', se   *
      *                   si vuole che tutto vada bene.                *
      *                   La clausola 'before 1' pero' fa' si', per    *
      *                   gli altri tipi di Runtime, che il file in-   *
      *                   teressato sia considerato un Print-File,     *
      *                   con conseguente impossibilita' di aprire     *
      *                   il file in i-o o in input.                   *
      *                   Da qui lo sdoppiamento in due files distin-  *
      *                   ti.                                          *
      *                                                                *
      *     ---------------------------------------------------------- *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Inizio scrittura record segreteria .               *
      * Output                                                         *
      *             Input  : b-ope = "OO"                              *
      *                                                                *
      *                      b-chr = Prefisso o postfisso unico per i  *
      *                              files temporanei                  *
      *                                                                *
      *                      s-num = Codice runtime in uso             *
      *                                                                *
      *             Output : b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error verificatosi *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put         Scrittura segmento del record segreteria .         *
      *                                                                *
      *             Input  : b-ope = "PT"                              *
      *                                                                *
      *                      b-chr = Segmento da 255  bytes del record *
      *                              segreteria                        *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Inizio lettura record segreteria .                 *
      * Input                                                          *
      *             Input  : b-ope = "OI"                              *
      *                                                                *
      *                      b-chr = Prefisso o postfisso unico per i  *
      *                              files temporanei                  *
      *                                                                *
      *                      s-num = Codice runtime in uso             *
      *                                                                *
      *             Output : b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error verificatosi *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Get         Lettura segmento del record segreteria .           *
      *                                                                *
      *             Input  : b-ope = "GT"                              *
      *                                                                *
      *             Output : b-chr = Segmento da 255  bytes del record *
      *                              segreteria                        *
      *                                                                *
      *                      b-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - ##     : fine record            *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Fine scrittura o lettura del record segreteria.    *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [sgr]                                        *
      *    *-----------------------------------------------------------*
           select optional   sgr   assign to input-output w-fls-sgr-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is w-fls-sgr-sts            .

      *    *===========================================================*
      *    * File Control [sgx]                                        *
      *    *-----------------------------------------------------------*
           select optional   sgx   assign to input-output w-fls-sgr-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is w-fls-sgr-sts            .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File description [sgr]                                    *
      *    *-----------------------------------------------------------*
       fd  sgr  label record omitted.
       01  rc-sgr.
           05  rc-sgr-chr occurs 32767    pic  x(01)                  .

      *    *===========================================================*
      *    * File description [sgx]                                    *
      *    *-----------------------------------------------------------*
       fd  sgx  label record omitted.
       01  rc-sgx.
           05  rc-sgx-chr occurs 32767    pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work area per trattamento file riguardante il cobol       *
      *    *-----------------------------------------------------------*
       01  w-fls-sgr.
      *        *-------------------------------------------------------*
      *        * Cobol file name                                       *
      *        *-------------------------------------------------------*
           05  w-fls-sgr-nam              pic  x(04) value "sgr "     .
      *        *-------------------------------------------------------*
      *        * Cobol file pathname                                   *
      *        *-------------------------------------------------------*
           05  w-fls-sgr-pat              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Cobol file status                                     *
      *        *-------------------------------------------------------*
           05  w-fls-sgr-sts              pic  x(02)                  .

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
      *    * Work per environment variables                            *
      *    *-----------------------------------------------------------*
       01  w-env-var.
      *        *-------------------------------------------------------*
      *        * Codice run-time in uso                                *
      *        * - 00 : Acucobol 85                                    *
      *        * - 01 : Austec RM-Master Cobol 74                      *
      *        * - 02 : Ryan McFarland RM-Cobol 85                     *
      *        *-------------------------------------------------------*
           05  w-env-var-run              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Salvataggio prefisso o postfisso unico per files tem- *
      *        * poranei                                               *
      *        *-------------------------------------------------------*
           05  w-ppu                      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tipo open in corso                            *
      *        *-------------------------------------------------------*
           05  w-top                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore per numero progressivo di segmento          *
      *        *-------------------------------------------------------*
           05  w-cns                      pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Puntatore per string-unstring su record segreteria    *
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

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using b
                                               s                      .
      ******************************************************************

      *    *===========================================================*
      *    * Declaratives                                              *
      *    *-----------------------------------------------------------*
       Declaratives.
       Decl Section.
           Use after standard error procedure on sgr
                                                 sgx                  .
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status ritornato  *
      *              * dal cobol nel codice di i-o status convenziona- *
      *              * le                                              *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using w-fls-sgr              .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      w-fls-sgr-sts        to   e-sts                  .
       End Declaratives.

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       Main Section.
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
      *              * Salvataggio codice runtime utilizzato           *
      *              *-------------------------------------------------*
           move      s-num                to   w-env-var-run          .
      *              *-------------------------------------------------*
      *              * Salvataggio prefisso o postfisso unico per i    *
      *              * files temporanei                                *
      *              *-------------------------------------------------*
           move      b-chr                to   w-ppu                  .
      *              *-------------------------------------------------*
      *              * Flag di tipo open in corso                      *
      *              *-------------------------------------------------*
           move      "O"                  to   w-top                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore per numero progres-  *
      *              * sivo di segmento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-cns                  .
      *              *-------------------------------------------------*
      *              * Preparazione pathname per il file contenente    *
      *              * l'immagine della segreteria                     *
      *              *-------------------------------------------------*
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
                     ".sgr"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-fls-sgr-pat          .
       opn-out-200.
      *              *-------------------------------------------------*
      *              * Deviazione secondo il runtime utilizzato        *
      *              *-------------------------------------------------*
           if        w-env-var-run        =    01
                     go to opn-out-600.
       opn-out-400.
      *              *-------------------------------------------------*
      *              * Se runtime : Acucobol 85          oppure        *
      *              *              Ryan Mc-Farland RM Cobol 85        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di i-o               *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o di open output            *
      *                  *---------------------------------------------*
           open      output sgr           with no rewind              .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione di i-o             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se nessun errore si continua normalmen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-out-450.
      *                      *-----------------------------------------*
      *                      * Altrimenti si ritorna lo status di er-  *
      *                      * rore                                    *
      *                      *-----------------------------------------*
           move      e-sts                to   b-rsc                  .
      *                      *-----------------------------------------*
      *                      * Quindi si esce                          *
      *                      *-----------------------------------------*
           go to     opn-out-999.
       opn-out-450.
      *                  *---------------------------------------------*
      *                  * Inizializzazione record segreteria          *
      *                  *---------------------------------------------*
           move      spaces               to   rc-sgr                 .
      *                  *---------------------------------------------*
      *                  * Status di uscita a OK                       *
      *                  *---------------------------------------------*
           move      spaces               to   b-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-out-999.
       opn-out-600.
      *              *-------------------------------------------------*
      *              * Se runtime : Austec RM Master Cobol 74          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di i-o               *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o di open output            *
      *                  *                                             *
      *                  * N.B.: comando inibito per evitare problemi  *
      *                  * di compilazione                             *
      *                  *---------------------------------------------*
______*    open      output sgx           with no rewind              .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione di i-o             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se nessun errore si continua normalmen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-out-650.
      *                      *-----------------------------------------*
      *                      * Altrimenti si ritorna lo status di er-  *
      *                      * rore                                    *
      *                      *-----------------------------------------*
           move      e-sts                to   b-rsc                  .
      *                      *-----------------------------------------*
      *                      * Quindi si esce                          *
      *                      *-----------------------------------------*
           go to     opn-out-999.
       opn-out-650.
      *                  *---------------------------------------------*
      *                  * Inizializzazione record segreteria          *
      *                  *---------------------------------------------*
           move      spaces               to   rc-sgx                 .
      *                  *---------------------------------------------*
      *                  * Status di uscita a OK                       *
      *                  *---------------------------------------------*
           move      spaces               to   b-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-out-999.
       opn-out-999.
           exit.

      *    *===========================================================*
      *    * Put segmento di record segreteria                         *
      *    *-----------------------------------------------------------*
       put-sgm-000.
      *              *-------------------------------------------------*
      *              * Status di uscita comunque a OK                  *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il runtime utilizzato        *
      *              *-------------------------------------------------*
           if        w-env-var-run        =    01
                     go to put-sgm-600.
       put-sgm-400.
      *              *-------------------------------------------------*
      *              * Se runtime : Acucobol 85          oppure        *
      *              *              Ryan Mc-Farland RM Cobol 85        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Concatenazione segmento in record segrete-  *
      *                  * ria                                         *
      *                  *---------------------------------------------*
           move      w-cns                to   w-pnt                  .
           multiply  255                  by   w-pnt                  .
           add       1                    to   w-pnt                  .
           add       1                    to   w-cns                  .
           move      b-chr                to   w-seu                  .
           string    w-seu
                     delimited by size    into rc-sgr
                                  with pointer w-pnt                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-sgm-999.
       put-sgm-600.
      *              *-------------------------------------------------*
      *              * Se runtime : Austec RM Master Cobol 74          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Concatenazione segmento in record segrete-  *
      *                  * ria                                         *
      *                  *---------------------------------------------*
           move      w-cns                to   w-pnt                  .
           multiply  255                  by   w-pnt                  .
           add       1                    to   w-pnt                  .
           add       1                    to   w-cns                  .
           move      b-chr                to   w-seu                  .
           string    w-seu
                     delimited by size    into rc-sgx
                                  with pointer w-pnt                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-sgm-999.
       put-sgm-999.
           exit.

      *    *===========================================================*
      *    * Open input                                                *
      *    *-----------------------------------------------------------*
       opn-inp-000.
      *              *-------------------------------------------------*
      *              * Salvataggio codice runtime utilizzato           *
      *              *-------------------------------------------------*
           move      s-num                to   w-env-var-run          .
      *              *-------------------------------------------------*
      *              * Salvataggio prefisso o postfisso unico per i    *
      *              * files temporanei                                *
      *              *-------------------------------------------------*
           move      b-chr                to   w-ppu                  .
      *              *-------------------------------------------------*
      *              * Preparazione pathname per il file contenente    *
      *              * l'immagine della segreteria                     *
      *              *-------------------------------------------------*
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
                     ".sgr"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-fls-sgr-pat          .
      *              *-------------------------------------------------*
      *              * Flag di tipo open in corso                      *
      *              *-------------------------------------------------*
           move      "I"                  to   w-top                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore per numero progres-  *
      *              * sivo di segmento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-cns                  .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il runtime utilizzato        *
      *              *-------------------------------------------------*
           if        w-env-var-run        =    01
                     go to opn-inp-600.
       opn-inp-400.
      *              *-------------------------------------------------*
      *              * Se runtime : Acucobol 85          oppure        *
      *              *              Ryan Mc-Farland RM Cobol 85        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di i-o               *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o di open input             *
      *                  *---------------------------------------------*
           open      input  sgr                                       .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione di i-o             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se nessun errore si continua normalmen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-inp-420.
      *                      *-----------------------------------------*
      *                      * Altrimenti si ritorna lo status di er-  *
      *                      * rore                                    *
      *                      *-----------------------------------------*
           move      e-sts                to   b-rsc                  .
      *                      *-----------------------------------------*
      *                      * Quindi si esce                          *
      *                      * rore                                    *
      *                      *-----------------------------------------*
           go to     opn-inp-999.
       opn-inp-420.
      *                  *---------------------------------------------*
      *                  * Lettura del record segreteria               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      spaces               to   rc-sgr                 .
      *                      *-----------------------------------------*
      *                      * Operazione di i-o di read               *
      *                      *-----------------------------------------*
           read      sgr    at end
                            move  e-end-fil      
                                          to   e-sts                  .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione di i-o         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se nessun errore si continua nor-   *
      *                          * malmente                            *
      *                          *-------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-inp-440.
      *                          *-------------------------------------*
      *                          * Altrimenti si ritorna lo status di  *
      *                          * errore                              *
      *                          *-------------------------------------*
           move      e-sts                to   b-rsc                  .
      *                          *-------------------------------------*
      *                          * Quindi si chiude il file            *
      *                          *-------------------------------------*
           close     sgr                                              .
      *                          *-------------------------------------*
      *                          * Quindi si esce                      *
      *                          *-------------------------------------*
           go to     opn-inp-999.
       opn-inp-440.
      *                  *---------------------------------------------*
      *                  * Status di uscita a OK                       *
      *                  *---------------------------------------------*
           move      spaces               to   b-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-inp-999.
       opn-inp-600.
      *              *-------------------------------------------------*
      *              * Se runtime : Austec RM Master Cobol 74          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di i-o               *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o di open input             *
      *                  *---------------------------------------------*
______*    open      input  sgx                                       .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione di i-o             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se nessun errore si continua normalmen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-inp-620.
      *                      *-----------------------------------------*
      *                      * Altrimenti si ritorna lo status di er-  *
      *                      * rore                                    *
      *                      *-----------------------------------------*
           move      e-sts                to   b-rsc                  .
      *                      *-----------------------------------------*
      *                      * Quindi si esce                          *
      *                      * rore                                    *
      *                      *-----------------------------------------*
           go to     opn-inp-999.
       opn-inp-620.
      *                  *---------------------------------------------*
      *                  * Lettura del record segreteria               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      spaces               to   rc-sgx                 .
      *                      *-----------------------------------------*
      *                      * Operazione di i-o di read               *
      *                      *-----------------------------------------*
           read      sgx    at end
                            move  e-end-fil      
                                          to   e-sts                  .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione di i-o         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se nessun errore si continua nor-   *
      *                          * malmente                            *
      *                          *-------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-inp-640.
      *                          *-------------------------------------*
      *                          * Altrimenti si ritorna lo status di  *
      *                          * errore                              *
      *                          *-------------------------------------*
           move      e-sts                to   b-rsc                  .
      *                          *-------------------------------------*
      *                          * Quindi si chiude il file            *
      *                          *-------------------------------------*
______*    close     sgx                                              .
      *                          *-------------------------------------*
      *                          * Quindi si esce                      *
      *                          *-------------------------------------*
           go to     opn-inp-999.
       opn-inp-640.
      *                  *---------------------------------------------*
      *                  * Status di uscita a OK                       *
      *                  *---------------------------------------------*
           move      spaces               to   b-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-inp-999.
       opn-inp-999.
           exit.

      *    *===========================================================*
      *    * Get segmento di record segreteria                         *
      *    *-----------------------------------------------------------*
       get-sgm-000.
      *              *-------------------------------------------------*
      *              * Status di uscita comunque a OK                  *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il runtime utilizzato        *
      *              *-------------------------------------------------*
           if        w-env-var-run        =    01
                     go to get-sgm-600.
       get-sgm-400.
      *              *-------------------------------------------------*
      *              * Se runtime : Acucobol 85          oppure        *
      *              *              Ryan Mc-Farland RM Cobol 85        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione del segmento dal record segrete- *
      *                  * ria                                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-seu                  .
           move      w-cns                to   w-pnt                  .
           multiply  255                  by   w-pnt                  .
           add       1                    to   w-pnt                  .
           add       1                    to   w-cns                  .
           move      w-pnt                to   w-svp                  .
           unstring  rc-sgr               into w-seu
                                  with pointer w-pnt                  .
           move      w-seu                to   b-chr                  .
           if        w-pnt                =    w-svp
                     move  "##"           to   b-rsc
           else      move  spaces         to   b-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-sgm-999.
       get-sgm-600.
      *              *-------------------------------------------------*
      *              * Se runtime : Austec RM Master Cobol 74          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione del segmento dal record segrete- *
      *                  * ria                                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-seu                  .
           move      w-cns                to   w-pnt                  .
           multiply  255                  by   w-pnt                  .
           add       1                    to   w-pnt                  .
           add       1                    to   w-cns                  .
           move      w-pnt                to   w-svp                  .
           unstring  rc-sgx               into w-seu
                                  with pointer w-pnt                  .
           move      w-seu                to   b-chr                  .
           if        w-pnt                =    w-svp
                     move  "##"           to   b-rsc
           else      move  spaces         to   b-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-sgm-999.
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
      *              * Normalizzazione status di i-o                   *
      *              *-------------------------------------------------*
           move      "00"                 to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di write                      *
      *              *-------------------------------------------------*
           perform   wrt-lin-seq-000      thru wrt-lin-seq-999        .
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun errore si continua normalmente    *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to cls-out-300.
      *                  *---------------------------------------------*
      *                  * Altrimenti si ritorna lo status di errore   *
      *                  *---------------------------------------------*
           move      e-sts                to   b-rsc                  .
           close     sgr                                              .
           go to     cls-out-999.
       cls-out-300.
      *              *-------------------------------------------------*
      *              * Operazione di i-o di close                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione secondo il runtime utilizzato    *
      *                  *---------------------------------------------*
           if        w-env-var-run        =    01
                     go to cls-out-360.
       cls-out-340.
      *                  *---------------------------------------------*
      *                  * Se runtime : Acucobol 85          oppure    *
      *                  *              Ryan Mc-Farland RM Cobol 85    *
      *                  *---------------------------------------------*
           close     sgr                                              .
           go to     cls-out-400.
       cls-out-360.
      *                  *---------------------------------------------*
      *                  * Se runtime : Austec RM Master Cobol 74      *
      *                  *---------------------------------------------*
           close     sgx                                              .
           go to     cls-out-400.
       cls-out-400.
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun errore si continua normalmente    *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to cls-out-600.
      *                  *---------------------------------------------*
      *                  * Altrimenti si ritorna lo status di errore   *
      *                  *---------------------------------------------*
           move      e-sts                to   b-rsc                  .
           go to     cls-out-999.
       cls-out-600.
      *              *-------------------------------------------------*
      *              * Status di uscita a OK                           *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
       cls-out-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di scrittura su file line sequential in fun-   *
      *    * zione del tipo di runtime utilizzato                      *
      *    *-----------------------------------------------------------*
       wrt-lin-seq-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di runtime        *
      *              *-------------------------------------------------*
           if        w-env-var-run        =    01
                     go to wrt-lin-seq-600.
       wrt-lin-seq-400.
      *              *-------------------------------------------------*
      *              * Se runtime : Acucobol 85          oppure        *
      *              *              Ryan Mc-Farland RM Cobol 85        *
      *              *-------------------------------------------------*
           write     rc-sgr                                           .
           go to     wrt-lin-seq-999.
       wrt-lin-seq-600.
      *              *-------------------------------------------------*
      *              * Se runtime : Austec RM Master Cobol 74          *
      *              *-------------------------------------------------*
           write     rc-sgx              before 1                     .
           go to     wrt-lin-seq-999.
       wrt-lin-seq-999.
           exit.

      *    *===========================================================*
      *    * Close input                                               *
      *    *-----------------------------------------------------------*
       cls-inp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di i-o                   *
      *              *-------------------------------------------------*
           move      "00"                 to   e-sts                  .
       cls-inp-300.
      *              *-------------------------------------------------*
      *              * Operazione di i-o di close                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione secondo il runtime utilizzato    *
      *                  *---------------------------------------------*
           if        w-env-var-run        =    01
                     go to cls-inp-360.
       cls-inp-340.
      *                  *---------------------------------------------*
      *                  * Se runtime : Acucobol 85          oppure    *
      *                  *              Ryan Mc-Farland RM Cobol 85    *
      *                  *---------------------------------------------*
           close     sgr                                              .
           go to     cls-inp-400.
       cls-inp-360.
      *                  *---------------------------------------------*
      *                  * Se runtime : Austec RM Master Cobol 74      *
      *                  *---------------------------------------------*
______*    close     sgx                                              .
           go to     cls-inp-400.
       cls-inp-400.
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun errore si continua normalmente    *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to cls-inp-800.
      *                  *---------------------------------------------*
      *                  * Altrimenti si ritorna lo status di errore   *
      *                  *---------------------------------------------*
           move      e-sts                to   b-rsc                  .
           go to     cls-inp-999.
       cls-inp-800.
      *              *-------------------------------------------------*
      *              * Status di uscita a OK                           *
      *              *-------------------------------------------------*
           move      spaces               to   b-rsc                  .
       cls-inp-999.
           exit.
