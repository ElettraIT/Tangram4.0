       Identification Division.
       Program-Id.                                 dbarcod0           .
      *================================================================*
      *                                                                *
      * Modulo per la scrittura, relativamente ad un prodotto di :     *
      *                                                                *
      * - Bar-code su file EPS di tipo EAN13 o 128 (numerico)          *
      *                                                                *
      * - Pathname : /abd/asc/bcd/99/XXXXXXXXXX.eps                    *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/06/06    *
      *                       Ultima revisione:    NdK del 23/02/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * NOTA BENE : per i codici 128 si puo' migliorare la gestione    *
      * ---------   dei numerici a seconda del numero caratteri !!!    *
      *             (pari o dispari, 'C' o 'A')                        *
      *                                                                *
      *             Migliorabile anche l'esclusione del codice leggi-  *
      *             bile (vedi link area)                              *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-bar-cod-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "AP" - Assemblaggio codice con prefisso da referenza           *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "AP"                       *
      *                                                                *
      *                 d-bar-cod-tip-cod = Tipo bar-code              *
      *                                                                *
      *                 d-bar-cod-tip-cdp = Tipo codice prodotto       *
      *                                                                *
      *                 d-bar-cod-num-cod = Codice numerico (opz)      *
      *                                                                *
      *                 d-bar-cod-alf-cod = Codice alfanumerico        *
      *                                                                *
      *                                                                *
      *        Output : d-bar-cod-bar-cod = Codice senza check digit   *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CD" - Determinazione del Check Digit                          *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "CD"                       *
      *                                                                *
      *                 d-bar-cod-tip-cod = Tipo bar-code              *
      *                                                                *
      *                 d-bar-cod-bar-cod = Bar-code senza check digit *
      *                                                                *
      *                                                                *
      *        Output : d-bar-cod-bar-cod = Codice completo di check   *
      *                                     digit                      *
      *                                                                *
      *                 d-bar-cod-chk-dgt = Check digit                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "MF" o "MB" - Costruzione del file EPS per il bar-code         *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "MF" o "MB"                *
      *                                                                *
      *                 d-bar-cod-tip-cdp = Tipo codice prodotto       *
      *                                                                *
      *                 d-bar-cod-num-cod = Codice numerico (opz)      *
      *                                                                *
      *                 d-bar-cod-alf-cod = Codice alfanumerico        *
      *                                                                *
      *                 d-bar-cod-bar-cod = Bar-code (opzionale)       *
      *                                                                *
      *                                                                *
      *        Output : d-bar-cod-exi-sts = Esito scrittura            *
      *                                                                *
      *                 d-bar-cod-buf-eps = Se tipo operazione "MB"    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DF" - Default bar-code gestito                                *
      *                                                                *
      *                                                                *
      *        Input  : d-bar-cod-tip-ope = "DF"                       *
      *                                                                *
      *                 d-bar-cod-tip-cod = Tipo bar-code              *
      *                                                                *
      *                                                                *
      *        Output : d-bar-cod-exi-sts = Tipo bar-code default      *
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
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Specifiche per bar-code                               *
      *        *-------------------------------------------------------*
           05  w-ref-spc-bcd.
      *            *---------------------------------------------------*
      *            * Stringa letta da referenza                        *
      *            *---------------------------------------------------*
               10  w-ref-spc-bcd-alf      pic  x(50)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione                                     *
      *            *---------------------------------------------------*
               10  w-ref-spc-bcd-bcd      pic  x(08)                  .
               10  w-ref-spc-bcd-pfx      pic  x(10)                  .
               10  w-ref-spc-bcd-aon      pic  x(01)                  .
               10  w-ref-spc-bcd-chr      pic  x(02)                  .
               10  w-ref-spc-bcd-pth      pic  x(20)                  .
               10  w-ref-spc-bcd-chp      pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione per valori numerici                 *
      *            *---------------------------------------------------*
               10  w-ref-spc-bcd-chp-r redefines
                   w-ref-spc-bcd-chp.
                   15  w-ref-spc-bcd-chn  pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) trailing
                                                     separate
                                                     character
                                                     value zero       .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-npe                  pic  x(13)                  .
           05  f-xxx-npx                  pic  x(04)                  .
           05  f-xxx-npc                  pic  x(05)                  .
           05  f-xxx-ppb                  pic  x(40)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work area per tabella ridefinizione 'code 128'            *
      *    *-----------------------------------------------------------*
           copy      "swd/drv/prg/cpy/w128cod0.cpw"                   .

      *    *===========================================================*
      *    * Work area per tabella ridefinizione 'EAN 13'              *
      *    *-----------------------------------------------------------*
           copy      "swd/drv/prg/cpy/weancod0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazione check digit per bar-code      *
      *        *-------------------------------------------------------*
           05  w-det-chk-dgt.
      *            *---------------------------------------------------*
      *            * Comodo per il trattamento                         *
      *            *---------------------------------------------------*
               10  w-det-chk-dgt-cod      pic  x(13)                  .
               10  w-det-chk-dgA-128 redefines
                   w-det-chk-dgt-cod.
                   15  w-det-chk-dgt-cnA  occurs 05
                                          pic  9(01)                  .
                   15  w-det-chk-dgt-flA  pic  x(08)                  .
               10  w-det-chk-dgC-128 redefines
                   w-det-chk-dgt-cod.
                   15  w-det-chk-dgt-cnC  occurs 03
                                          pic  9(02)                  .
                   15  w-det-chk-dgt-flC  pic  x(07)                  .
               10  w-det-chk-dgt-num redefines
                   w-det-chk-dgt-cod.
                   15  w-det-chk-dgt-cnn  pic  9(12)                  .
                   15  w-det-chk-dgt-fl2  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatori e flag                                  *
      *            *---------------------------------------------------*
               10  w-det-chk-dgt-fpd      pic  x(01)                  .
               10  w-det-chk-dgt-c01      pic  9(02)                  .
               10  w-det-chk-dgt-par      pic  9(03)                  .
               10  w-det-chk-dgt-dsp      pic  9(03)                  .
               10  w-det-chk-dgt-n01      pic  9(01)                  .
               10  w-det-chk-dgt-pa3      pic  9(03)                  .
               10  w-det-chk-dgt-tot      pic  9(03)                  .
               10  w-det-chk-dgt-tch      pic  9(01)                  .
               10  w-det-chk-dgt-dcd      pic  x(10)    value
                                                         "0123456789" .
               10  w-det-chk-dgt-t00      pic  9(09)                  .
               10  w-det-chk-dgt-t01      pic  9(09)                  .
               10  w-det-chk-dgt-t02      pic  9(09)                  .
               10  w-det-chk-dgt-t03      pic  9(09)                  .
               10  w-det-chk-dgt-t04      pic  9(03)                  .
               10  w-det-chk-dgt-t05      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Check digit                                       *
      *            *---------------------------------------------------*
               10  w-det-chk-dgt-cdg      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione validita' del codice          *
      *        *-------------------------------------------------------*
           05  w-det-chk-val.
      *            *---------------------------------------------------*
      *            * Flag                                              *
      *            *---------------------------------------------------*
               10  w-det-chk-val-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per scrittura file EPS                           *
      *        *-------------------------------------------------------*
           05  w-mak-fil-eps.
      *            *---------------------------------------------------*
      *            * Comodi per il trattamento                         *
      *            *---------------------------------------------------*
               10  w-mak-fil-eps-cod      pic  x(13)                  .
               10  w-mak-fil-eps-red redefines
                   w-mak-fil-eps-cod.
                   15  w-mak-fil-eps-chr occurs 13
                                          pic  x(01)                  .
               10  w-mak-fil-eps-mwd      pic  x(02) value "40"       .
      *        *-------------------------------------------------------*
      *        * Per decodifica EAN 13                                 *
      *        *-------------------------------------------------------*
           05  w-det-chr-e13.
      *            *---------------------------------------------------*
      *            * Indicatore di posizione                           *
      *            *---------------------------------------------------*
               10  w-det-chr-e13-pos      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Carattere da trattare                             *
      *            *---------------------------------------------------*
               10  w-det-chr-e13-chr      pic  x(01)                  .
               10  w-det-chr-e13-red redefines
                   w-det-chr-e13-chr.
                   15  w-det-chr-e13-num  pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Stringa determinata                               *
      *            *---------------------------------------------------*
               10  w-det-chr-e13-str      pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-det-chr-e13-aeb      pic  x(06)                  .
               10  w-det-chr-e13-ffe      pic  9(02)                  .
               10  w-det-chr-e13-pnt      pic  9(02)                  .
               10  w-det-chr-e13-pn2      pic  9(02)                  .
               10  w-det-chr-e13-c01      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per decodifica Code 128                               *
      *        *-------------------------------------------------------*
           05  w-det-chr-128.
      *            *---------------------------------------------------*
      *            * Indicatore di posizione                           *
      *            *---------------------------------------------------*
               10  w-det-chr-128-pos      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Stringa determinata                               *
      *            *---------------------------------------------------*
               10  w-det-chr-128-str      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-det-chr-128-pnt      pic  9(03)                  .
               10  w-det-chr-128-chs      pic  x(01)                  .
               10  w-det-chr-128-set      pic  x(06)                  .
               10  w-det-chr-128-set-r redefines
                   w-det-chr-128-set.
                   15  w-det-chr-128-num  occurs 6
                                          pic  9(01)                  .
               10  w-det-chr-128-c01      pic  9(02)                  .
               10  w-det-chr-128-c02      pic  9(02)                  .
               10  w-det-chr-128-c03      pic  9(02)                  .
               10  w-det-chr-128-chd      pic  x(03)                  .
               10  w-det-chr-128-chd-r redefines
                   w-det-chr-128-chd.
                   15  w-det-chr-128-chn  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione buffer di output              *
      *        *-------------------------------------------------------*
           05  w-det-buf-eps.
      *            *---------------------------------------------------*
      *            * Comodi per il trattamento                         *
      *            *---------------------------------------------------*
               10  w-det-buf-eps-ctr      pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per il driver                                   *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione system date and time           *
      *        *-------------------------------------------------------*
           05  w-sdt                      pic  9(15)                  .
           05  w-sdt-r01 redefines w-sdt.
               10  w-sdt-ann              pic  9(03)                  .
               10  w-sdt-mes              pic  9(02)                  .
               10  w-sdt-gio              pic  9(02)                  .
               10  w-sdt-ora              pic  9(02)                  .
               10  w-sdt-min              pic  9(02)                  .
               10  w-sdt-sec              pic  9(02)                  .
               10  w-sdt-cnt              pic  9(02)                  .
           05  w-sdt-r02 redefines w-sdt.
               10  w-sdt-xxx occurs 15    pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per commento 'CreationDate'                      *
      *        *-------------------------------------------------------*
           05  w-crd.
               10  w-crd-mmm              pic  9(02)                  .
               10  w-crd-t01              pic  x(01) value "-"        .
               10  w-crd-ddd              pic  9(02)                  .
               10  w-crd-t02              pic  x(01) value "-"        .
               10  w-crd-yyy              pic  9(04)                  .
               10  w-crd-t03              pic  x(01) value spaces     .
               10  w-crd-hhh              pic  9(02)                  .
               10  w-crd-t04              pic  x(01) value ":"        .
               10  w-crd-min              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione bar-code         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/dbarcod0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-bar-cod              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-bar-cod-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio con prefisso                   *
      *                  *---------------------------------------------*
           if        d-bar-cod-tip-ope    =    "AP"
                     perform ass-pre-cod-000
                                          thru ass-pre-cod-999
      *                  *---------------------------------------------*
      *                  * Determinazione Check Digit                  *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "CD"
                     perform det-chk-dgt-000
                                          thru det-chk-dgt-999
      *                  *---------------------------------------------*
      *                  * Scrittura file EPS                          *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "MF"
                     perform mak-fil-eps-000
                                          thru mak-fil-eps-999
      *                  *---------------------------------------------*
      *                  * Scrittura buffer EPS                        *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "MB"
                     perform mak-buf-eps-000
                                          thru mak-buf-eps-999
      *                  *---------------------------------------------*
      *                  * Default tipo bar-code                       *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "DF"
                     perform det-def-bcd-000
                                          thru det-def-bcd-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-bar-cod-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore per buffer            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-buf-eps-ctr      .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Specifiche per bar-code                     *
      *                  *---------------------------------------------*
           perform   ref-spc-bcd-000      thru ref-spc-bcd-999        .
       rou-opn-fls-400.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
       rou-opn-fls-800.
      *              *-------------------------------------------------*
      *              * Preparazione pathname di base files sequenziali *
      *              *-------------------------------------------------*
           perform   pre-bas-pth-000      thru pre-bas-pth-999        .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura della referenza relativa alle specifiche per i    *
      *    * bar-code                                                  *
      *    *-----------------------------------------------------------*
       ref-spc-bcd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione referenza                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-spc-bcd          .
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/dcp[spc-bcd]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione referenza                       *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-spc-bcd-alf
           else      go to ref-spc-bcd-999.
      *              *-------------------------------------------------*
      *              * Estrazione dei valori                           *
      *              *-------------------------------------------------*
           move      w-ref-spc-bcd-alf    to   w-all-str-alf          .
           move      ";"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-ref-spc-bcd-bcd      .
           move      w-all-str-cat (2)    to   w-ref-spc-bcd-pfx      .
           move      w-all-str-cat (3)    to   w-ref-spc-bcd-aon      .
           move      w-all-str-cat (4)    to   w-ref-spc-bcd-chr      .
           move      w-all-str-cat (5)    to   w-ref-spc-bcd-pth      .
           move      w-all-str-cat (6)    to   w-ref-spc-bcd-chp      .
      *              *-------------------------------------------------*
      *              * Controllo dei valori estratti                   *
      *              *-------------------------------------------------*
           if        w-ref-spc-bcd-pfx    =    spaces
                     move  "100000"       to   w-ref-spc-bcd-pfx      .
      *
           if        w-ref-spc-bcd-pth    =    spaces
                     move  "bcd/"         to   w-ref-spc-bcd-pth      .
      *
           if        w-ref-spc-bcd-chn    not  numeric
                     move  zero           to   w-ref-spc-bcd-chn      .
       ref-spc-bcd-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
       rou-cls-fls-200.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
       rou-cls-fls-800.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tst-cnc-mod-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   d-bar-cod-exi-sts
           else      move  "#"            to   d-bar-cod-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Assemblaggio prefisso con codice                          *
      *    *-----------------------------------------------------------*
       ass-pre-cod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      spaces               to   d-bar-cod-bar-cod      .
      *              *-------------------------------------------------*
      *              * Test su tipo codice prodotto                    *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    not  = "A"  and
                     d-bar-cod-tip-cdp    not  = "N"
                     move  "A"            to   d-bar-cod-tip-cdp      .
      *              *-------------------------------------------------*
      *              * Test su valore codice alfanumerico prodotto     *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "A"    and
                     d-bar-cod-alf-cod    =    spaces
                     go to ass-pre-cod-900.
      *              *-------------------------------------------------*
      *              * Test su valore codice numerico prodotto         *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "N"    and
                     d-bar-cod-num-cod    =    zero
                     go to ass-pre-cod-900.
      *              *-------------------------------------------------*
      *              * Test su valore referenza prefisso EAN           *
      *              *-------------------------------------------------*
           if        w-ref-spc-bcd-pfx    =    spaces
                     go to ass-pre-cod-900.
       ass-pre-cod-100.
      *              *-------------------------------------------------*
      *              * Eventuale editing del codice numerico in alfa-  *
      *              * numerico                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul tipo codice                        *
      *                  *---------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "A"
                     go to ass-pre-cod-200.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      d-bar-cod-num-cod    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   d-bar-cod-alf-cod      .
       ass-pre-cod-200.
      *              *-------------------------------------------------*
      *              * Riempimento in base alla lunghezza del codice   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza codice             *
      *                  *---------------------------------------------*
           move      d-bar-cod-alf-cod    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
      *                  *---------------------------------------------*
      *                  * Taglio del prefisso in base alla lunghezza  *
      *                  * del codice                                  *
      *                  *---------------------------------------------*
           move      12                   to   w-all-str-inx          .
           subtract  w-all-str-lun        from w-all-str-inx          .
       ass-pre-cod-300.
      *              *-------------------------------------------------*
      *              * Assemblaggio del prefisso con il codice         *
      *              *-------------------------------------------------*
           move      13                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-ref-spc-bcd-pfx
                    (01 : w-all-str-inx)  to   w-all-str-cat (1)      .
           move      d-bar-cod-alf-cod    to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   d-bar-cod-bar-cod      .
       ass-pre-cod-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ass-pre-cod-999.
       ass-pre-cod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del check digit                            *
      *    *-----------------------------------------------------------*
       det-chk-dgt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      spaces               to   d-bar-cod-chk-dgt      .
       det-chk-dgt-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo bar-code        *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cod    =    "EAN13STD"
                     perform det-chk-dgt-e13-000
                                          thru det-chk-dgt-e13-999
           else if   d-bar-cod-tip-cod    =    "C128N05A"
                     perform det-chk-dgt-128-000
                                          thru det-chk-dgt-128-999
           else if   d-bar-cod-tip-cod    =    "C128N06C"
                     perform det-chk-dgt-128-000
                                          thru det-chk-dgt-128-999
           else      go to det-chk-dgt-900.
       det-chk-dgt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-dgt-999.
       det-chk-dgt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del check digit di una stringa numerica    *
      *    *                                                           *
      *    * EAN 13                                                    *
      *    *-----------------------------------------------------------*
       det-chk-dgt-e13-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del check digit                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-chk-dgt-cdg      .
      *              *-------------------------------------------------*
      *              * Valore in input in comodo di trattamento        *
      *              *-------------------------------------------------*
           move      d-bar-cod-bar-cod    to   w-det-chk-dgt-cod      .
      *              *-------------------------------------------------*
      *              * Test su stringa in input                        *
      *              *-------------------------------------------------*
           if        w-det-chk-dgt-cod    =    spaces
                     go to det-chk-dgt-e13-900.
           if        w-det-chk-dgt-cnn    not  numeric
                     go to det-chk-dgt-e13-900.
       det-chk-dgt-e13-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione della stringa                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-chk-dgt-fpd      .
           move      zero                 to   w-det-chk-dgt-c01      .
           move      zero                 to   w-det-chk-dgt-pa3      .
           move      zero                 to   w-det-chk-dgt-par      .
           move      zero                 to   w-det-chk-dgt-dsp      .
           move      zero                 to   w-det-chk-dgt-tot      .
       det-chk-dgt-e13-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-chk-dgt-c01      .
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-chk-dgt-c01    >    13
                     go to det-chk-dgt-e13-400.
      *                  *---------------------------------------------*
      *                  * Test se carattere numerico                  *
      *                  *---------------------------------------------*
           if        w-det-chk-dgt-cod
                    (w-det-chk-dgt-c01 : 1)
                                          not numeric
                     go to det-chk-dgt-e13-400.
      *                  *---------------------------------------------*
      *                  * Carattere numerico in comodo di trattamento *
      *                  *---------------------------------------------*
           move      w-det-chk-dgt-cod
                    (w-det-chk-dgt-c01 : 1)
                                          to   w-det-chk-dgt-n01      .
      *                  *---------------------------------------------*
      *                  * Incremento totalizzatore pari o dispari     *
      *                  *---------------------------------------------*
           if        w-det-chk-dgt-fpd    =    spaces
                     add  w-det-chk-dgt-n01
                                          to   w-det-chk-dgt-dsp
                     move  "#"            to   w-det-chk-dgt-fpd
           else      add  w-det-chk-dgt-n01
                                          to   w-det-chk-dgt-par
                     move  spaces         to   w-det-chk-dgt-fpd      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-chk-dgt-e13-200.
       det-chk-dgt-e13-400.
      *              *-------------------------------------------------*
      *              * Operazioni su totalizzatori pari o dispari      *
      *              *-------------------------------------------------*
           multiply  3                    by   w-det-chk-dgt-par
                                        giving w-det-chk-dgt-pa3      .
           add       w-det-chk-dgt-pa3    to   w-det-chk-dgt-dsp
                                        giving w-det-chk-dgt-tot      .
      *              *-------------------------------------------------*
      *              * Ciclo per raggiungere un multiplo di 10         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-chk-dgt-c01      .
       det-chk-dgt-e13-500.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-chk-dgt-c01      .
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-chk-dgt-c01    >    10
                     go to det-chk-dgt-e13-600.
      *                  *---------------------------------------------*
      *                  * Totale in campo di comodo                   *
      *                  *---------------------------------------------*
           move      w-det-chk-dgt-tot    to   w-det-chk-dgt-tch      .
      *                  *---------------------------------------------*
      *                  * Test se raggiunto '0' nell'ultima cifra     *
      *                  *---------------------------------------------*
           if        w-det-chk-dgt-tch    =    zero
                     go to det-chk-dgt-e13-600.
      *                  *---------------------------------------------*
      *                  * Incremento del totale per raggiungere 10    *
      *                  *---------------------------------------------*
           add       1                    to   w-det-chk-dgt-tot      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-chk-dgt-e13-500.
       det-chk-dgt-e13-600.
      *              *-------------------------------------------------*
      *              * Assegnazione del check digit alfanumerico       *
      *              *-------------------------------------------------*
           move      w-det-chk-dgt-dcd
                    (w-det-chk-dgt-c01 : 1)
                                          to   w-det-chk-dgt-cdg      .
       det-chk-dgt-e13-800.
      *              *-------------------------------------------------*
      *              * Valori in output                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Check Digit                                 *
      *                  *---------------------------------------------*
           move      w-det-chk-dgt-cdg    to   d-bar-cod-chk-dgt      .
      *                  *---------------------------------------------*
      *                  * Bar-code completo                           *
      *                  *---------------------------------------------*
           move      13                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-det-chk-dgt-cod    to   w-all-str-cat (1)      .
           move      w-det-chk-dgt-cdg    to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   d-bar-cod-bar-cod      .
       det-chk-dgt-e13-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-dgt-e13-999.
       det-chk-dgt-e13-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del check digit di una stringa numerica    *
      *    *                                                           *
      *    * CODE 128                                                  *
      *    *-----------------------------------------------------------*
       det-chk-dgt-128-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo codice          *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cod    =    "C128N05A"
                     perform det-chk-dgA-128-000
                                          thru det-chk-dgA-128-999
           else if   d-bar-cod-tip-cod    =    "C128N06C"
                     perform det-chk-dgC-128-000
                                          thru det-chk-dgC-128-999
           else      go to det-chk-dgt-128-900.
       det-chk-dgt-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-dgt-128-999.
       det-chk-dgt-128-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del check digit di una stringa numerica    *
      *    *                                                           *
      *    * CODE 128 (numerico di 5) Tipo 'A'                         *
      *    *-----------------------------------------------------------*
       det-chk-dgA-128-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del check digit                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-chk-dgt-cdg      .
      *              *-------------------------------------------------*
      *              * Valore in input in comodo di trattamento        *
      *              *-------------------------------------------------*
           move      d-bar-cod-alf-cod    to   w-det-chk-dgt-cod      .
      *              *-------------------------------------------------*
      *              * Test su stringa in input                        *
      *              *-------------------------------------------------*
           if        w-det-chk-dgt-cod    =    spaces
                     go to det-chk-dgA-128-900.
           if        w-det-chk-dgt-cnA (01)
                                          not  numeric or
                     w-det-chk-dgt-cnA (02)
                                          not  numeric or
                     w-det-chk-dgt-cnA (03)
                                          not  numeric or
                     w-det-chk-dgt-cnA (04)
                                          not  numeric or
                     w-det-chk-dgt-cnA (05)
                                          not  numeric
                     go to det-chk-dgA-128-900.
       det-chk-dgA-128-100.
      *              *-------------------------------------------------*
      *              * Determinazione check digit per un codice nume-  *
      *              * rico di 5 cifre                                 *
      *              *                                                 *
      *              * Componenti :                                    *
      *              *                                                 *
      *              * Start digit              = 103   (set.'A')      *
      *              * 1. valore    "9"         = nn  x 1              *
      *              * 2. valore    "9"         = nn  x 2              *
      *              * 3. valore    "9"         = nn  x 3              *
      *              * 4. valore    "9"         = nn  x 4              *
      *              * 5. valore    "9"         = nn  x 5              *
      *              *                                                 *
      *              * Si sommano tutti i risultati delle moltiplica-  *
      *              * zioni ed il totale si divide per 103.           *
      *              * Si moltiplica 103 per l'intero risultante dalla *
      *              * divisione e si sottrae il risultato dal totale  *
      *              * precedente.                                     *
      *              *                                                 *
      *              * ----------------------------------------------- *
      *              *                                                 *
      *              * Codice di esempio = 12345                       *
      *              *                                                 *
      *              * Start digit        =  103 (set.'A')             *
      *              * 1. valore    "1"   =  17 x 1 =  17 +            *
      *              * 2. valore    "2"   =  18 x 2 =  36 +            *
      *              * 3. valore    "3"   =  19 x 3 =  57 =            *
      *              * 4. valore    "4"   =  20 x 4 =  80 =            *
      *              * 5. valore    "5"   =  21 x 5 = 105 =            *
      *              *                                ---              *
      *              *                                398              *
      *              *                                                 *
      *              * 398 : 103 = 3,86407                             *
      *              * 103 x 3   = 309                                 *
      *              * 398 - 309 = 89                                  *
      *              * 89        = check digit (EM)                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start digit (A type)                        *
      *                  *---------------------------------------------*
           move      103                  to   w-det-chk-dgt-t00      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodi                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-chk-dgt-t01      .
           move      zero                 to   w-det-chk-dgt-c01      .
       det-chk-dgA-128-200.
      *                  *---------------------------------------------*
      *                  * Ciclo di cumulo progressivo                 *
      *                  *---------------------------------------------*
           add       1                    to   w-det-chk-dgt-c01      .
           if        w-det-chk-dgt-c01    >    5
                     go to det-chk-dgA-128-300.
           move      w-det-chk-dgt-cnA
                    (w-det-chk-dgt-c01)   to   w-det-chk-dgt-t05      .
           add       16                   to   w-det-chk-dgt-t05      .
           multiply  w-det-chk-dgt-c01    by   w-det-chk-dgt-t05
                                        giving w-det-chk-dgt-t01      .
           add       w-det-chk-dgt-t01    to   w-det-chk-dgt-t00      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-chk-dgA-128-200.
       det-chk-dgA-128-300.
      *                  *---------------------------------------------*
      *                  * Divisione per 103                           *
      *                  *---------------------------------------------*
           divide    103                  into w-det-chk-dgt-t00
                                        giving w-det-chk-dgt-t02      .
      *                  *---------------------------------------------*
      *                  * Moltiplicazione per 103                     *
      *                  *---------------------------------------------*
           multiply  103                  by   w-det-chk-dgt-t02
                                        giving w-det-chk-dgt-t03      .
           subtract  w-det-chk-dgt-t03    from w-det-chk-dgt-t00
                                        giving w-det-chk-dgt-t04      .
           move      w-det-chk-dgt-t04
                    (01 : 03)             to   w-det-chk-dgt-cdg      .
       det-chk-dgA-128-800.
      *              *-------------------------------------------------*
      *              * Valori in output                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Check Digit                                 *
      *                  *---------------------------------------------*
           move      w-det-chk-dgt-cdg    to   d-bar-cod-chk-dgt      .
       det-chk-dgA-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-dgA-128-999.
       det-chk-dgA-128-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del check digit di una stringa numerica    *
      *    *                                                           *
      *    * CODE 128 (numerico di 6) Tipo 'C'                         *
      *    *-----------------------------------------------------------*
       det-chk-dgC-128-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del check digit                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-chk-dgt-cdg      .
      *              *-------------------------------------------------*
      *              * Valore in input in comodo di trattamento        *
      *              *-------------------------------------------------*
           move      d-bar-cod-alf-cod    to   w-det-chk-dgt-cod      .
      *              *-------------------------------------------------*
      *              * Test su stringa in input                        *
      *              *-------------------------------------------------*
           if        w-det-chk-dgt-cod    =    spaces
                     go to det-chk-dgC-128-900.
           if        w-det-chk-dgt-cnC (01)
                                          not  numeric or
                     w-det-chk-dgt-cnC (02)
                                          not  numeric or
                     w-det-chk-dgt-cnC (03)
                                          not  numeric
                     go to det-chk-dgC-128-900.
       det-chk-dgC-128-100.
      *              *-------------------------------------------------*
      *              * Determinazione check digit per un codice nume-  *
      *              * rico di 6 cifre                                 *
      *              *                                                 *
      *              * Componenti :                                    *
      *              *                                                 *
      *              * Start digit              = 105   (set.'C')      *
      *              * 1. coppia    "99"        = nn  x 1              *
      *              * 2. coppia    "99"        = nn  x 2              *
      *              * 3. coppia    "99"        = nn  x 3              *
      *              *                                                 *
      *              * Si sommano tutti i risultati delle moltiplica-  *
      *              * zioni ed il totale si divide per 103.           *
      *              * Si moltiplica 103 per l'intero risultante dalla *
      *              * divisione e si sottrae il risultato dal totale  *
      *              * precedente.                                     *
      *              *                                                 *
      *              * ----------------------------------------------- *
      *              *                                                 *
      *              * Codice di esempio = 062084                      *
      *              *                                                 *
      *              * Start digit        = 105 (set.'C')              *
      *              * 1. coppia    "06"  =   6 x 1 =   6 +            *
      *              * 2. coppia    "20"  =  20 x 2 =  40 +            *
      *              * 3. coppia    "84"  =  84 x 3 = 252 =            *
      *              *                                ---              *
      *              *                                298              *
      *              *                                                 *
      *              * 298 : 103 = 2,8932                              *
      *              * 103 x 2   = 206                                 *
      *              * 298 - 206 = 92                                  *
      *              * 92        = check digit (FS)                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start digit (C type)                        *
      *                  *---------------------------------------------*
           move      105                  to   w-det-chk-dgt-t00      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodi                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-chk-dgt-t01      .
           move      zero                 to   w-det-chk-dgt-c01      .
       det-chk-dgC-128-200.
      *                  *---------------------------------------------*
      *                  * Ciclo di cumulo progressivo                 *
      *                  *---------------------------------------------*
           add       1                    to   w-det-chk-dgt-c01      .
           if        w-det-chk-dgt-c01    >    3
                     go to det-chk-dgC-128-300.
           multiply  w-det-chk-dgt-c01    by   w-det-chk-dgt-cnC
                                              (w-det-chk-dgt-c01)
                                        giving w-det-chk-dgt-t01      .
           add       w-det-chk-dgt-t01    to   w-det-chk-dgt-t00      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-chk-dgC-128-200.
       det-chk-dgC-128-300.
      *                  *---------------------------------------------*
      *                  * Divisione per 103                           *
      *                  *---------------------------------------------*
           divide    103                  into w-det-chk-dgt-t00
                                        giving w-det-chk-dgt-t02      .
      *                  *---------------------------------------------*
      *                  * Moltiplicazione per 103                     *
      *                  *---------------------------------------------*
           multiply  103                  by   w-det-chk-dgt-t02
                                        giving w-det-chk-dgt-t03      .
           subtract  w-det-chk-dgt-t03    from w-det-chk-dgt-t00
                                        giving w-det-chk-dgt-t04      .
           move      w-det-chk-dgt-t04
                    (01 : 03)             to   w-det-chk-dgt-cdg      .
       det-chk-dgC-128-800.
      *              *-------------------------------------------------*
      *              * Valori in output                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Check Digit                                 *
      *                  *---------------------------------------------*
           move      w-det-chk-dgt-cdg    to   d-bar-cod-chk-dgt      .
       det-chk-dgC-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-dgC-128-999.
       det-chk-dgC-128-999.
           exit.

      *    *===========================================================*
      *    * Check preliminare di validita' codice                     *
      *    *-----------------------------------------------------------*
       det-chk-val-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-chk-val-flg      .
       det-chk-val-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo bar-code        *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cod    =    "EAN13STD"
                     perform det-chk-val-e13-000
                                          thru det-chk-val-e13-999
           else if   d-bar-cod-tip-cod    =    "C128N05A"
                     perform det-chk-vaA-128-000
                                          thru det-chk-vaA-128-999
           else if   d-bar-cod-tip-cod    =    "C128N06C"
                     perform det-chk-vaC-128-000
                                          thru det-chk-vaC-128-999
           else      go to det-chk-val-900.
       det-chk-val-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-val-999.
       det-chk-val-999.
           exit.

      *    *===========================================================*
      *    * Check preliminare di validita' codice                     *
      *    *                                                           *
      *    * EAN 13                                                    *
      *    *-----------------------------------------------------------*
       det-chk-val-e13-000.
      *              *-------------------------------------------------*
      *              * Valore in input in comodo di trattamento        *
      *              *-------------------------------------------------*
           move      d-bar-cod-bar-cod    to   w-det-chk-dgt-cod      .
      *              *-------------------------------------------------*
      *              * Test se valore interamente numerico             *
      *              *-------------------------------------------------*
           if        w-det-chk-dgt-cnn    not  numeric
                     move  "#"            to   w-det-chk-val-flg      .
       det-chk-val-e13-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-val-e13-999.
       det-chk-val-e13-999.
           exit.

      *    *===========================================================*
      *    * Check preliminare di validita' codice                     *
      *    *                                                           *
      *    * CODE 128 (numerico di 5)                                  *
      *    *-----------------------------------------------------------*
       det-chk-vaA-128-000.
      *              *-------------------------------------------------*
      *              * Valore in input in comodo di trattamento        *
      *              *-------------------------------------------------*
           move      d-bar-cod-alf-cod    to   w-det-chk-dgt-cod      .
      *              *-------------------------------------------------*
      *              * Test se valore interamente numerico             *
      *              *-------------------------------------------------*
           if        w-det-chk-dgt-cnA (01)
                                          not  numeric or
                     w-det-chk-dgt-cnA (02)
                                          not  numeric or
                     w-det-chk-dgt-cnA (03)
                                          not  numeric or
                     w-det-chk-dgt-cnA (04)
                                          not  numeric or
                     w-det-chk-dgt-cnA (05)
                                          not  numeric
                     move  "#"            to   w-det-chk-val-flg      .
       det-chk-vaA-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-vaA-128-999.
       det-chk-vaA-128-999.
           exit.

      *    *===========================================================*
      *    * Check preliminare di validita' codice                     *
      *    *                                                           *
      *    * CODE 128 (numerico di 6)                                  *
      *    *-----------------------------------------------------------*
       det-chk-vaC-128-000.
      *              *-------------------------------------------------*
      *              * Valore in input in comodo di trattamento        *
      *              *-------------------------------------------------*
           move      d-bar-cod-alf-cod    to   w-det-chk-dgt-cod      .
      *              *-------------------------------------------------*
      *              * Test se valore interamente numerico             *
      *              *-------------------------------------------------*
           if        w-det-chk-dgt-cnC (01)
                                          not  numeric or
                     w-det-chk-dgt-cnC (02)
                                          not  numeric or
                     w-det-chk-dgt-cnC (03)
                                          not  numeric
                     move  "#"            to   w-det-chk-val-flg      .
       det-chk-vaC-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chk-vaC-128-999.
       det-chk-vaC-128-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *-----------------------------------------------------------*
       mak-fil-eps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      spaces               to   d-bar-cod-exi-sts      .
       mak-fil-eps-010.
      *              *-------------------------------------------------*
      *              * Test su tipo codice prodotto                    *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    not  = "A"  and
                     d-bar-cod-tip-cdp    not  = "N"
                     move  "A"            to   d-bar-cod-tip-cdp      .
      *              *-------------------------------------------------*
      *              * Test su valore codice alfanumerico prodotto     *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "A"    and
                     d-bar-cod-alf-cod    =    spaces
                     move  "#"            to   d-bar-cod-exi-sts
                     go to mak-fil-eps-900.
      *              *-------------------------------------------------*
      *              * Test su valore codice numerico prodotto         *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "N"    and
                     d-bar-cod-num-cod    =    zero
                     move  "#"            to   d-bar-cod-exi-sts
                     go to mak-fil-eps-900.
       mak-fil-eps-030.
      *              *-------------------------------------------------*
      *              * Check preliminare dei codici                    *
      *              *-------------------------------------------------*
           perform   det-chk-val-000      thru det-chk-val-999        .
           if        w-det-chk-val-flg    not  = spaces
                     move  "#"            to   d-bar-cod-exi-sts
                     go to mak-fil-eps-900.
       mak-fil-eps-050.
      *              *-------------------------------------------------*
      *              * Preparazione file name                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione infunzione del tipo codice       *
      *                  *---------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "A"
                     go to mak-fil-eps-070
           else      go to mak-fil-eps-080.
       mak-fil-eps-070.
      *                  *---------------------------------------------*
      *                  * Se codice alfanumerico                      *
      *                  *---------------------------------------------*
           move      d-bar-cod-alf-cod    to   f-xxx-npe              .
      *                  *---------------------------------------------*
      *                  * A open file                                 *
      *                  *---------------------------------------------*
           go to     mak-fil-eps-100.
       mak-fil-eps-080.
      *                  *---------------------------------------------*
      *                  * Se codice numerico                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      d-bar-cod-num-cod    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   f-xxx-npe              .
      *                  *---------------------------------------------*
      *                  * A open file                                 *
      *                  *---------------------------------------------*
           go to     mak-fil-eps-100.
       mak-fil-eps-100.
      *              *-------------------------------------------------*
      *              * Open generica file sequenziale di output        *
      *              *-------------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
       mak-fil-eps-300.
      *              *-------------------------------------------------*
      *              * Scrittura vera e propria                        *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-wrt-000  thru mak-fil-eps-wrt-999    .
       mak-fil-eps-800.
      *              *-------------------------------------------------*
      *              * Chiusura del file in output                     *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       mak-fil-eps-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mak-fil-eps-999.
       mak-fil-eps-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS su buffer                          *
      *    *-----------------------------------------------------------*
       mak-buf-eps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      spaces               to   d-bar-cod-buf-eps      .
       mak-buf-eps-010.
      *              *-------------------------------------------------*
      *              * Test su tipo codice prodotto                    *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    not  = "A"  and
                     d-bar-cod-tip-cdp    not  = "N"
                     move  "A"            to   d-bar-cod-tip-cdp      .
      *              *-------------------------------------------------*
      *              * Test su valore codice alfanumerico prodotto     *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "A"    and
                     d-bar-cod-alf-cod    =    spaces
                     move  "#"            to   d-bar-cod-exi-sts
                     go to mak-buf-eps-900.
      *              *-------------------------------------------------*
      *              * Test su valore codice numerico prodotto         *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cdp    =    "N"    and
                     d-bar-cod-num-cod    =    zero
                     move  "#"            to   d-bar-cod-exi-sts
                     go to mak-buf-eps-900.
       mak-buf-eps-030.
      *              *-------------------------------------------------*
      *              * Check preliminare dei codici                    *
      *              *-------------------------------------------------*
           perform   det-chk-val-000      thru det-chk-val-999        .
           if        w-det-chk-val-flg    not  = spaces
                     move  "#"            to   d-bar-cod-exi-sts
                     go to mak-buf-eps-900.
      *              *-------------------------------------------------*
      *              * Scrittura vera e propria                        *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-wrt-000  thru mak-fil-eps-wrt-999    .
       mak-buf-eps-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mak-buf-eps-999.
       mak-buf-eps-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Subroutine per scrittura                                  *
      *    *-----------------------------------------------------------*
       mak-fil-eps-wrt-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo bar-code        *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-cod    =    "EAN13STD"
                     perform mak-fil-eps-e13-000
                                          thru mak-fil-eps-e13-999
           else if   d-bar-cod-tip-cod    =    "C128N05A"
                     perform mak-fil-epA-128-000
                                          thru mak-fil-epA-128-999
           else if   d-bar-cod-tip-cod    =    "C128N06C"
                     perform mak-fil-epC-128-000
                                          thru mak-fil-epC-128-999
           else      go to mak-fil-eps-wrt-900.
       mak-fil-eps-wrt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mak-fil-eps-wrt-999.
       mak-fil-eps-wrt-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Subroutine per scrittura EAN 13 Standard                  *
      *    *-----------------------------------------------------------*
       mak-fil-eps-e13-000.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       mak-fil-eps-e13-010.
      *              *-------------------------------------------------*
      *              * Ridefinizione ean-code                          *
      *              *-------------------------------------------------*
           move      d-bar-cod-bar-cod    to   w-mak-fil-eps-cod      .
       mak-fil-eps-e13-100.
      *              *-------------------------------------------------*
      *              * 'Header Comments'                               *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-hea-000  thru mak-fil-eps-hea-999    .
       mak-fil-eps-e13-200.
      *              *-------------------------------------------------*
      *              * Definizione variabili                           *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-var-000  thru mak-fil-eps-var-999    .
       mak-fil-eps-e13-300.
      *              *-------------------------------------------------*
      *              * Definizione procedure                           *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-prc-000  thru mak-fil-eps-prc-999    .
       mak-fil-eps-e13-350.
      *              *-------------------------------------------------*
      *              * Definizione procedure specifiche                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cifre leggibili (allineate a sinistra)      *
      *                  *                                             *
      *                  *  - EAN13 Left Digits                        *
      *                  *---------------------------------------------*
           move      "moduleWidth 24 mul 0 rmoveto"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      spaces               to   g-rec                  .
           string    "moduleWidth 40 mul ("
                                delimited by   size
                     w-mak-fil-eps-cod (02 : 06)
                                delimited by   spaces
                     ")"        delimited by   size
                                          into g-rec                  .
      *
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/Helvetica fitstring topcentershow"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Primo carattere a sinistra, fuori barcode   *
      *                  *                                             *
      *                  *  - EAN13 Flag Digit                         *
      *                  *---------------------------------------------*
           move      "BcdX BcdY moveto"   to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      spaces               to   g-rec                  .
           string    "-1 0 rmoveto ("
                                delimited by   size
                     w-mak-fil-eps-chr (1)
                                delimited by   spaces
                     ") toprightshow"
                                delimited by   size
                                          into g-rec                  .
      *
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Cifre leggibili (allineate a destra)        *
      *                  *                                             *
      *                  *  - EAN13 Right Digits                       *
      *                  *---------------------------------------------*
           move      "BcdX BcdY moveto"   to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "moduleWidth 70 mul 0 rmoveto"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      spaces               to   g-rec                  .
           string    "moduleWidth 40 mul ("
                                delimited by   size
                     w-mak-fil-eps-cod (08 : 06)
                                delimited by   spaces
                     ") topcentershow"
                                delimited by   size
                                          into g-rec                  .
      *
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "} def"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-e13-400.
      *              *-------------------------------------------------*
      *              * Stampa bar-code                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      "PreMakeBcd"         to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Left guard bars                             *
      *                  *---------------------------------------------*
           move      "L O L"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-e13-410.
      *                  *---------------------------------------------*
      *                  * Ciclo 1..12                                 *
      *                  *---------------------------------------------*
      *                       *----------------------------------------*
      *                       * Preparazione                           *
      *                       *----------------------------------------*
           move      zero                 to   w-det-chr-e13-c01      .
       mak-fil-eps-e13-420.
           add       1                    to   w-det-chr-e13-c01      .
      *                       *----------------------------------------*
      *                       * 1..12                                  *
      *                       *----------------------------------------*
           if        w-det-chr-e13-c01    >    13
                     go to mak-fil-eps-e13-600.
      *                       *----------------------------------------*
      *                       * Decodifica                             *
      *                       *----------------------------------------*
           move      w-det-chr-e13-c01    to   w-det-chr-e13-pos      .
           move      w-mak-fil-eps-chr
                    (w-det-chr-e13-c01)   to   w-det-chr-e13-chr      .
           perform   det-chr-e13-000      thru det-chr-e13-999        .
      *                       *----------------------------------------*
      *                       * Emissione                              *
      *                       *----------------------------------------*
           if        w-det-chr-e13-str    =    spaces
                     go to mak-fil-eps-e13-420.
           move      w-det-chr-e13-str    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                       *----------------------------------------*
      *                       * Test se centro                         *
      *                       *----------------------------------------*
           if        w-det-chr-e13-c01    not  = 7
                     go to mak-fil-eps-e13-480.
      *                       *----------------------------------------*
      *                       * Center guard bars                      *
      *                       *----------------------------------------*
           move      "O L O L O"          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-e13-480.
      *                       *----------------------------------------*
      *                       * Riciclo                                *
      *                       *----------------------------------------*
           go to     mak-fil-eps-e13-420.
       mak-fil-eps-e13-600.
      *                  *---------------------------------------------*
      *                  * Right guard bars                            *
      *                  *---------------------------------------------*
           move      "L O L"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Conclusione                                 *
      *                  *---------------------------------------------*
           move      "PostMakeBcd"        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-e13-800.
      *              *-------------------------------------------------*
      *              * Chiusura EPS                                    *
      *              *-------------------------------------------------*
           move      "showpage"           to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "%%EOF"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-e13-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mak-fil-eps-e13-999.
       mak-fil-eps-e13-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Subroutine per scrittura CODE 128 (numerico di 5 - 'A')   *
      *    *-----------------------------------------------------------*
       mak-fil-epA-128-000.
      *              *-------------------------------------------------*
      *              * Check digit                                     *
      *              *-------------------------------------------------*
           perform   det-chk-dgt-128-000  thru det-chk-dgt-128-999    .
       mak-fil-epA-128-010.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       mak-fil-epA-128-100.
      *              *-------------------------------------------------*
      *              * 'Header Comments'                               *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-hea-000  thru mak-fil-eps-hea-999    .
       mak-fil-epA-128-200.
      *              *-------------------------------------------------*
      *              * Definizione variabili                           *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-var-000  thru mak-fil-eps-var-999    .
       mak-fil-epA-128-300.
      *              *-------------------------------------------------*
      *              * Definizione procedure                           *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-prc-000  thru mak-fil-eps-prc-999    .
       mak-fil-epA-128-350.
      *              *-------------------------------------------------*
      *              * Definizione parametri per codice da stampare    *
      *              * sotto al barcode                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cifra leggibile (posizionamento verticale)  *
      *                  *                                             *
      *                  * N.B.: il parametro '0 rmoveto' indica la    *
      *                  *       posizione Y della cifra leggibile     *
      *                  *       rispetto al barcode                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri                               *
      *                      *-----------------------------------------*
           move      "moduleWidth 24 mul 0 rmoveto"
                                          to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Emissione linea                         *
      *                      *-----------------------------------------*
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Cifra leggibile (allineata a sinistra)      *
      *                  *                                             *
      *                  * N.B.: per ritarare la cifra leggibile, si   *
      *                  *       deve modificare 'moduleWidth 40 mul'. *
      *                  *                                             *
      *                  *       Per eliminarla, bisogna emettere la   *
      *                  *       stringa 'moduleWidth 40 mul ( )',     *
      *                  *       oppure il moltiplicatore (mul) a zero *
      *                  *                                             *
      *                  *       [DA IMPLEMENTARE in link area]        *
      *                  *                                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      spaces               to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Test se codice da stampare a spazi      *
      *                      *-----------------------------------------*
           if        d-bar-cod-alf-cod    =    spaces
                     move "moduleWidth 40 mul ( )"
                                          to  g-rec
                     go to mak-fil-epA-128-370.
      *                      *-----------------------------------------*
      *                      * Se codice da stampare                   *
      *                      *-----------------------------------------*
           string    "moduleWidth 40 mul ("
                                delimited by   size
                     d-bar-cod-alf-cod
                                delimited by   spaces
                     ")"        delimited by   size
                                          into g-rec                  .
       mak-fil-epA-128-370.
      *                      *-----------------------------------------*
      *                      * Emissione linea                         *
      *                      *-----------------------------------------*
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epA-128-380.
      *                  *---------------------------------------------*
      *                  * Posizionamento scritta e font               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri                               *
      *                      *-----------------------------------------*
           move      "/Helvetica fitstring topcentershow"
                                          to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Emissione linea                         *
      *                      *-----------------------------------------*
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura definizione testo barcode          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri                               *
      *                      *-----------------------------------------*
           move      "} def"              to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Emissione linea                         *
      *                      *-----------------------------------------*
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epA-128-400.
      *              *-------------------------------------------------*
      *              * Stampa bar-code                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      "PreMakeBcd"         to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Start character ('A' type) [value 103]      *
      *                  *                                             *
      *                  * 2 1 1 4 1 2                                 *
      *                  *---------------------------------------------*
           move      "I I O I O O O O I O O"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epA-128-410.
      *                  *---------------------------------------------*
      *                  * Ciclo 1..5                                  *
      *                  *---------------------------------------------*
      *                       *----------------------------------------*
      *                       * Preparazione                           *
      *                       *----------------------------------------*
           move      zero                 to   w-det-chr-128-c01      .
       mak-fil-epA-128-420.
           add       1                    to   w-det-chr-128-c01      .
      *                       *----------------------------------------*
      *                       * 1..5                                   *
      *                       *----------------------------------------*
           if        w-det-chr-128-c01    >    5
                     go to mak-fil-epA-128-600.
      *                       *----------------------------------------*
      *                       * Decodifica                             *
      *                       *----------------------------------------*
           move      w-det-chk-dgt-cnA
                    (w-det-chr-128-c01)   to   w-det-chr-128-pnt      .
           add       17                   to   w-det-chr-128-pnt      .
           move      w-128-tbl-sbx
                    (w-det-chr-128-pnt)   to   w-det-chr-128-set      .
           perform   det-chr-128-000      thru det-chr-128-999        .
      *                       *----------------------------------------*
      *                       * Emissione                              *
      *                       *----------------------------------------*
           if        w-det-chr-128-str    =    spaces
                     go to mak-fil-epA-128-420.
           move      w-det-chr-128-str    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                       *----------------------------------------*
      *                       * Riciclo                                *
      *                       *----------------------------------------*
           go to     mak-fil-epA-128-420.
       mak-fil-epA-128-600.
      *                  *---------------------------------------------*
      *                  * Check Digit                                 *
      *                  *---------------------------------------------*
      *                       *----------------------------------------*
      *                       * Decodifica                             *
      *                       *----------------------------------------*
           move      w-det-chk-dgt-cdg    to   w-det-chr-128-chd      .
           move      w-det-chr-128-chn    to   w-det-chr-128-pnt      .
           add       1                    to   w-det-chr-128-pnt      .
           move      w-128-tbl-sbx
                    (w-det-chr-128-pnt)   to   w-det-chr-128-set      .
           perform   det-chr-128-000      thru det-chr-128-999        .
      *                       *----------------------------------------*
      *                       * Emissione                              *
      *                       *----------------------------------------*
           if        w-det-chr-128-str    =    spaces
                     go to mak-fil-epA-128-700.
           move      w-det-chr-128-str    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epA-128-700.
      *                  *---------------------------------------------*
      *                  * Stop character                              *
      *                  *                                             *
      *                  * 2 3 3 1 1 1 2                               *
      *                  *---------------------------------------------*
           move      "I I O O O I I I O I O I I"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Conclusione                                 *
      *                  *---------------------------------------------*
           move      "PostMakeBcd"        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epA-128-800.
      *              *-------------------------------------------------*
      *              * Chiusura EPS                                    *
      *              *-------------------------------------------------*
           move      "showpage"           to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "%%EOF"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epA-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mak-fil-epA-128-999.
       mak-fil-epA-128-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Subroutine per scrittura CODE 128 (numerico di 6 - 'C')   *
      *    *-----------------------------------------------------------*
       mak-fil-epC-128-000.
      *              *-------------------------------------------------*
      *              * Check digit                                     *
      *              *-------------------------------------------------*
           perform   det-chk-dgt-128-000  thru det-chk-dgt-128-999    .
       mak-fil-epC-128-010.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       mak-fil-epC-128-100.
      *              *-------------------------------------------------*
      *              * 'Header Comments'                               *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-hea-000  thru mak-fil-eps-hea-999    .
       mak-fil-epC-128-200.
      *              *-------------------------------------------------*
      *              * Definizione variabili                           *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-var-000  thru mak-fil-eps-var-999    .
       mak-fil-epC-128-300.
      *              *-------------------------------------------------*
      *              * Definizione procedure                           *
      *              *-------------------------------------------------*
           perform   mak-fil-eps-prc-000  thru mak-fil-eps-prc-999    .
       mak-fil-epC-128-350.
      *              *-------------------------------------------------*
      *              * Definizione procedure specifiche                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cifra leggibile (allineata a sinistra)      *
      *                  *                                             *
      *                  * N.B.: per ritarare la cifra leggibile, si   *
      *                  *       deve modificare 'moduleWidth 40 mul'  *
      *                  *---------------------------------------------*
           move      "moduleWidth 24 mul 0 rmoveto"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      spaces               to   g-rec                  .
           string    "moduleWidth 40 mul ("
                                delimited by   size
                     d-bar-cod-alf-cod
                                delimited by   spaces
                     ")"        delimited by   size
                                          into g-rec                  .
      *
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/Helvetica fitstring topcentershow"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "} def"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epC-128-400.
      *              *-------------------------------------------------*
      *              * Stampa bar-code                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      "PreMakeBcd"         to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Start character ('C' type) [value 105]      *
      *                  *                                             *
      *                  * 2 1 1 2 3 2                                 *
      *                  *---------------------------------------------*
           move      "I I O I O O I I I O O"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epC-128-410.
      *                  *---------------------------------------------*
      *                  * Ciclo 1..3                                  *
      *                  *---------------------------------------------*
      *                       *----------------------------------------*
      *                       * Preparazione                           *
      *                       *----------------------------------------*
           move      zero                 to   w-det-chr-128-c01      .
       mak-fil-epC-128-420.
           add       1                    to   w-det-chr-128-c01      .
      *                       *----------------------------------------*
      *                       * 1..3                                   *
      *                       *----------------------------------------*
           if        w-det-chr-128-c01    >    3
                     go to mak-fil-epC-128-600.
      *                       *----------------------------------------*
      *                       * Decodifica                             *
      *                       *----------------------------------------*
           move      w-det-chk-dgt-cnC
                    (w-det-chr-128-c01)   to   w-det-chr-128-pnt      .
           add       1                    to   w-det-chr-128-pnt      .
           move      w-128-tbl-sbx
                    (w-det-chr-128-pnt)   to   w-det-chr-128-set      .
           perform   det-chr-128-000      thru det-chr-128-999        .
      *                       *----------------------------------------*
      *                       * Emissione                              *
      *                       *----------------------------------------*
           if        w-det-chr-128-str    =    spaces
                     go to mak-fil-epC-128-420.
           move      w-det-chr-128-str    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                       *----------------------------------------*
      *                       * Riciclo                                *
      *                       *----------------------------------------*
           go to     mak-fil-epC-128-420.
       mak-fil-epC-128-600.
      *                  *---------------------------------------------*
      *                  * Check Digit                                 *
      *                  *---------------------------------------------*
      *                       *----------------------------------------*
      *                       * Decodifica                             *
      *                       *----------------------------------------*
           move      w-det-chk-dgt-cdg    to   w-det-chr-128-chd      .
           move      w-det-chr-128-chn    to   w-det-chr-128-pnt      .
           add       1                    to   w-det-chr-128-pnt      .
           move      w-128-tbl-sbx
                    (w-det-chr-128-pnt)   to   w-det-chr-128-set      .
           perform   det-chr-128-000      thru det-chr-128-999        .
      *                       *----------------------------------------*
      *                       * Emissione                              *
      *                       *----------------------------------------*
           if        w-det-chr-128-str    =    spaces
                     go to mak-fil-epC-128-700.
           move      w-det-chr-128-str    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epC-128-700.
      *                  *---------------------------------------------*
      *                  * Stop character                              *
      *                  *                                             *
      *                  * 2 3 3 1 1 1 2                               *
      *                  *---------------------------------------------*
           move      "I I O O O I I I O I O I I"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Conclusione                                 *
      *                  *---------------------------------------------*
           move      "PostMakeBcd"        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epC-128-800.
      *              *-------------------------------------------------*
      *              * Chiusura EPS                                    *
      *              *-------------------------------------------------*
           move      "showpage"           to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "%%EOF"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-epC-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mak-fil-epC-128-999.
       mak-fil-epC-128-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Elementi comuni : 'Header Comments'                       *
      *    *-----------------------------------------------------------*
       mak-fil-eps-hea-000.
      *              *-------------------------------------------------*
      *              * 'Header Comments'                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * 'PS-Adobe'                                  *
      *                  *---------------------------------------------*
           move      "%!PS-Adobe-2.0 EPSF-1.2"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'Title'                                     *
      *                  *---------------------------------------------*
           move      spaces               to   g-rec                  .
           string    "%%Title: "
                                delimited by   size
                     d-bar-cod-alf-cod
                                delimited by   spaces
                                          into g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'Copyright'                                 *
      *                  *---------------------------------------------*
           move      "%%Copyright: wip - Sistemi Informativi Aziendali -
      -              " Padova - Italia"   to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'Creator'                                   *
      *                  *---------------------------------------------*
           move      spaces               to   g-rec                  .
      *
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           string    "%%Creator: "
                                delimited by   size
                     s-ute      delimited by   spaces
                     "-"        delimited by   size
                     s-ter      delimited by   spaces
                                          into g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'CreationDate'                              *
      *                  *                                             *
      *                  *  - Data gg-mm-aaaa                          *
      *                  *  - Ora  hh:mm                               *
      *                  *---------------------------------------------*
           move      spaces               to   g-rec                  .
      *
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      s-sdt                to   w-sdt                  .
           move      w-sdt-mes            to   w-crd-mmm              .
           move      w-sdt-gio            to   w-crd-ddd              .
           move      w-sdt-ann            to   w-crd-yyy              .
           add       1900                 to   w-crd-yyy              .
           move      w-sdt-ora            to   w-crd-hhh              .
           move      w-sdt-min            to   w-crd-min              .
           string    "%%CreationDate: "
                                delimited by   size
                     w-crd
                                delimited by   size
                                          into g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'For'                                       *
      *                  *---------------------------------------------*
           move      spaces               to   g-rec                  .
      *
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           string    "%%For: "
                                delimited by   size
                     s-azi      delimited by   spaces
                                          into g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'Pages'                                     *
      *                  *---------------------------------------------*
           move      "%%Pages: (atend)"   to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'BoundingBox'                               *
      *                  *---------------------------------------------*
           move      "%%BoundingBox: 0 0 100 40"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * 'EndComments'                               *
      *                  *---------------------------------------------*
           move      "%%EndComments"      to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-hea-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Elementi comuni : Variabili                               *
      *    *-----------------------------------------------------------*
       mak-fil-eps-var-000.
      *              *-------------------------------------------------*
      *              * Definizione variabili                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Formattazione font                          *
      *                  *---------------------------------------------*
           move      "/BcdFont { /Helvetica findfont 8 scalefont setfont
      -              " } def"             to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/fitstring {"       to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "dup findfont 1 scalefont setfont"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "3 1 roll"           to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "dup stringwidth pop"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "3 2 roll exch div"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "3 2 roll findfont exch scalefont setfont"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "} def"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Densita' barcode                            *
      *                  *---------------------------------------------*
           move      "/moduleWidth { 0.013000 72 mul } def"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Altezza barcode                             *
      *                  *                                             *
      *                  * N.B.: agendo su 'barMul' si puo' variare    *
      *                  *       l'altezza (range 0.2 - 1.0)           *
      *                  *---------------------------------------------*
           move      "/barMul 0.3 def"    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/barHeight { barMul 72 mul } def"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-var-999.
           exit.

      *    *===========================================================*
      *    * Scrittura del file EPS                                    *
      *    *                                                           *
      *    * Elementi comuni : Procedure                               *
      *    *-----------------------------------------------------------*
       mak-fil-eps-prc-000.
      *              *-------------------------------------------------*
      *              * Definizione procedure                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nuovo modulo                                *
      *                  *---------------------------------------------*
           move      "/nextModule { moduleWidth 0 rmoveto } def"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/topcentershow {dup stringwidth pop neg 2 div -9 r
      -              "moveto show} def"   to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/toprightshow {dup stringwidth pop neg -9 rmoveto 
      -              "show} def"          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Definizione moduli                          *
      *                  *                                             *
      *                  *  - 'I' = neri                               *
      *                  *  - 'O' = spazi bianchi                      *
      *                  *  - 'L' = neri e lunghi                      *
      *                  *---------------------------------------------*
           move      "/I { 0 barHeight rlineto 0 barHeight neg rmoveto n
      -              "extModule } def"    to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/L { 0 -5 rmoveto 0 5 rlineto I } def"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/O { nextModule } def"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Posizionamento bar-code                     *
      *                  *---------------------------------------------*
           move      "/BcdY 0010 def"     to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/BcdX {BcdY 0.1 add} def"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Pre-trattamento bar-code                    *
      *                  *---------------------------------------------*
           move      "/PreMakeBcd {"      to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "BcdFont"            to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "moduleWidth setlinewidth"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "BcdX BcdY moveto"   to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "gsave"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "} def"              to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Post-trattamento bar-code                   *
      *                  *---------------------------------------------*
           move      "/PostMakeBcd {"     to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "stroke"             to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "grestore"           to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       mak-fil-eps-prc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione decodifica singolo carattere EAN 13        *
      *    *-----------------------------------------------------------*
       det-chr-e13-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-chr-e13-str      .
       det-chr-e13-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-det-chr-e13-chr    =    spaces
                     go to det-chr-e13-900.
           if        w-det-chr-e13-num    not numeric
                     go to det-chr-e13-900.
       det-chr-e13-150.
      *              *-------------------------------------------------*
      *              * Bufferizzazioni preliminari                     *
      *              *-------------------------------------------------*
           if        w-det-chr-e13-pos    =    1
                     move  w-det-chr-e13-num
                                          to   w-det-chr-e13-ffe
                     add   1              to   w-det-chr-e13-ffe
                     go to det-chr-e13-900.
      *              *-------------------------------------------------*
      *              * Valore trattato in puntatore                    *
      *              *-------------------------------------------------*
           move      w-det-chr-e13-num    to   w-det-chr-e13-pnt      .
           add       1                    to   w-det-chr-e13-pnt      .
       det-chr-e13-200.
      *              *-------------------------------------------------*
      *              * Test se la cifra in corso di trattamento deve   *
      *              * essere posta a destra o sinistra                *
      *              *-------------------------------------------------*
           if        w-det-chr-e13-pos    >    7
                     go to det-chr-e13-500.
       det-chr-e13-300.
      *              *-------------------------------------------------*
      *              * Trattamento parte sinistra                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del First Flag       *
      *                  *---------------------------------------------*
           move      w-ean-ffe-ele-str
                    (w-det-chr-e13-ffe)   to   w-det-chr-e13-aeb      .
      *                  *---------------------------------------------*
      *                  * Scansione della stringa ottenuta            *
      *                  *---------------------------------------------*
           move      w-det-chr-e13-pos    to   w-det-chr-e13-pn2      .
           subtract  1                    from w-det-chr-e13-pn2      .
           if        w-det-chr-e13-aeb
                    (w-det-chr-e13-pn2 :
                     01)                  =    "A"
                     go to det-chr-e13-400
           else      go to det-chr-e13-450.
       det-chr-e13-400.
      *                  *---------------------------------------------*
      *                  * 'A'                                         *
      *                  *---------------------------------------------*
           move      w-ean-lha-ele-str
                    (w-det-chr-e13-pnt)   to   w-det-chr-e13-str      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-chr-e13-900.
       det-chr-e13-450.
      *                  *---------------------------------------------*
      *                  * 'B'                                         *
      *                  *---------------------------------------------*
           move      w-ean-lhb-ele-str
                    (w-det-chr-e13-pnt)   to   w-det-chr-e13-str      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-chr-e13-900.
       det-chr-e13-500.
      *              *-------------------------------------------------*
      *              * Trattamento parte destra                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      w-ean-rhx-ele-str
                    (w-det-chr-e13-pnt)   to   w-det-chr-e13-str      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-chr-e13-900.
       det-chr-e13-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chr-e13-999.
       det-chr-e13-999.
           exit.

      *    *===========================================================*
      *    * Determinazione decodifica singolo carattere Code 128      *
      *    *-----------------------------------------------------------*
       det-chr-128-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-chr-128-str      .
           move      zero                 to   w-det-chr-128-pos      .
       det-chr-128-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione del set (1..6) con riferi-   *
      *              * mento alla tabella di decodifica per Code 128   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-chr-128-c02      .
       det-chr-128-200.
           add       1                    to   w-det-chr-128-c02      .
           if        w-det-chr-128-c02    >    6
                     go to det-chr-128-900.
       det-chr-128-300.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione elemento del set (1..4)      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di incremento stringa                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-chr-128-c03      .
       det-chr-128-320.
           add       1                    to   w-det-chr-128-c03      .
           if        w-det-chr-128-c03    >    w-det-chr-128-num
                                              (w-det-chr-128-c02)
                     go to det-chr-128-600.
           if        w-det-chr-128-c03    >    4
                     go to det-chr-128-600.
      *                  *---------------------------------------------*
      *                  * Pari o dispari                              *
      *                  *---------------------------------------------*
           if        w-det-chr-128-c02    =    01 or
                     w-det-chr-128-c02    =    03 or
                     w-det-chr-128-c02    =    05
                     move  "I"            to   w-det-chr-128-chs
           else      move  "O"            to   w-det-chr-128-chs      .
      *                  *---------------------------------------------*
      *                  * Indice di posizionamento                    *
      *                  *---------------------------------------------*
           if        w-det-chr-128-c02    =    1 and
                     w-det-chr-128-c03    =    1
                     move  01             to   w-det-chr-128-pos
           else      add   02             to   w-det-chr-128-pos      .
      *                  *---------------------------------------------*
      *                  * Costruzione stringa                         *
      *                  *---------------------------------------------*
           move      w-det-chr-128-chs    to   w-det-chr-128-str
                                              (w-det-chr-128-pos : 01).
       det-chr-128-380.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-chr-128-320.
       det-chr-128-600.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-chr-128-200.
       det-chr-128-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chr-128-999.
       det-chr-128-999.
           exit.

      *    *===========================================================*
      *    * Determinazione default bar-code                           *
      *    *-----------------------------------------------------------*
       det-def-bcd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      spaces               to   d-bar-cod-tip-cod      .
       det-def-bcd-100.
      *              *-------------------------------------------------*
      *              * Valore letto da referenza                       *
      *              *-------------------------------------------------*
           move      w-ref-spc-bcd-bcd    to   d-bar-cod-tip-cod      .
       det-def-bcd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-def-bcd-999.
       det-def-bcd-999.
           exit.

      *    *===========================================================*
      *    * Open generica file sequenziale di output                  *
      *    *-----------------------------------------------------------*
       opn-seq-out-000.
      *              *-------------------------------------------------*
      *              * Eventuale estrazione di parte del codice pro-   *
      *              * dotto per la composizione del pathname          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   f-xxx-npc              .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-ref-spc-bcd-chn    =    zero
                     go to opn-seq-out-200.
           if        w-ref-spc-bcd-chn    >    12
                     go to opn-seq-out-200.
      *                  *---------------------------------------------*
      *                  * Estrazione del codice                       *
      *                  *---------------------------------------------*
           move      f-xxx-npe
                    (01 : w-ref-spc-bcd-chn)
                                          to   f-xxx-npc              .
      *                  *---------------------------------------------*
      *                  * Assemblaggio finale con '/'                 *
      *                  *---------------------------------------------*
           move      05                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      f-xxx-npc            to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   f-xxx-npc              .
       opn-seq-out-200.
      *              *-------------------------------------------------*
      *              * Preparazione pathname                           *
      *              *-------------------------------------------------*
           move      ".eps"               to   f-xxx-npx              .
           move      spaces               to   f-xxx-pat              .
           string    f-xxx-ppb  delimited by   spaces
                     w-ref-spc-bcd-pth
                                delimited by   spaces
                     f-xxx-npc  delimited by   spaces
                     f-xxx-npe  delimited by   spaces
                     f-xxx-npx  delimited by   spaces
                                          into f-xxx-pat              .
      *              *-------------------------------------------------*
      *              * Apertura del file in output                     *
      *              *-------------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      "seq "               to   g-nam                  .
           move      f-xxx-pat            to   g-pat                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       opn-seq-out-999.
           exit.

      *    *===========================================================*
      *    * Put next generica per output in sequenziale               *
      *    *-----------------------------------------------------------*
       put-nxt-out-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        d-bar-cod-tip-ope    =    "MB"
                     go to put-nxt-out-300
           else      go to put-nxt-out-500.
       put-nxt-out-300.
      *              *-------------------------------------------------*
      *              * Se scrittura su buffer                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore per buffer             *
      *                  *---------------------------------------------*
           add       1                    to   w-det-buf-eps-ctr      .
      *                  *---------------------------------------------*
      *                  * Scrittura su buffer                         *
      *                  *---------------------------------------------*
           move      g-rec                to   d-bar-cod-buf-txt
                                              (w-det-buf-eps-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     put-nxt-out-900.
       put-nxt-out-500.
      *              *-------------------------------------------------*
      *              * Se scrittura su sequenziale                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura su sequenziale                    *
      *                  *---------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     put-nxt-out-900.
       put-nxt-out-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     put-nxt-out-999.
       put-nxt-out-999.
           exit.

      *    *===========================================================*
      *    * Preparazione pathname di base files sequenziali           *
      *    *-----------------------------------------------------------*
       pre-bas-pth-000.
      *              *-------------------------------------------------*
      *              * Pathname di base da segreteria                  *
      *              *-------------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "asc "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * String                                          *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-ppb              .
           string    s-pat      delimited by   spaces
                     "/"        delimited by   size
                                          into f-xxx-ppb              .
       pre-bas-pth-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa o per l'estrazione di strin-  *
      *    * ghe separate da delimitatore                              *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


