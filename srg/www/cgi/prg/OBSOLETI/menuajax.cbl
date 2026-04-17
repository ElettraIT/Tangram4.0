       Identification Division.
       Program-Id.                                 menuajax           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    menuajax            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/12/03    *
      *                       Ultima revisione:    NdK del 11/02/14    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Prova di login a Tangram via Browser        *
      *                                                                *
      *                    Costruzione del menu                        *
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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "maucmf"  *
      *    *                                                 "mppssf"  *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [sss]                                                 *
      *        *-------------------------------------------------------*
           copy      "www/cgi/fls/rec/rfsss"                          .

      *    *===========================================================*
      *    * Work per records di [auc] 'ute'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucute0.cpw"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Flag di visualizzazione testata                       *
      *        *-------------------------------------------------------*
           05  w-exe-flg-tst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-ute              pic  x(08)                  .
           05  w-exe-pwd-ute              pic  x(08)                  .
           05  w-exe-win-nam              pic  x(32)                  .
           05  w-exe-flg-sts              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-001              pic  x(16)                  .
           05  w-exe-prm-002              pic  x(16)                  .
           05  w-exe-prm-003              pic  x(36)                  .
           05  w-exe-prm-004              pic  x(05)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division                                             .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           perform   ext-prm-000          thru ext-prm-999            .
      *              *-------------------------------------------------*
      *              * Modulo da eseguire                              *
      *              *-------------------------------------------------*
           perform   exe-mod-000          thru exe-mod-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *-----------------------------------------------------------*
       ext-prm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-prm-001          .
           move      spaces               to   w-exe-prm-002          .
           move      spaces               to   w-exe-prm-003          .
           move      spaces               to   w-exe-prm-004          .
      *              *-------------------------------------------------*
      *              * Data di sistema                                 *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * CORRETTIVO PROVVISORIO                          *
      *              *-------------------------------------------------*
           if        w-exe-dat-exe        <    999999
                     add  1000000         to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * PROBLEMA : 'o-shs' e' di 220 ma la variabile    *
      *              *            'POST' è attualmente di 99 caratteri *
      *              *                                                 *
      *              * ___                                             *
      *              *-------------------------------------------------*
      
      
      
      *              *-------------------------------------------------*
      *              * Estrazione 4 coppie parametro / valore          *
      *              *-------------------------------------------------*
           move      o-shs                to   w-all-str-alf          .
           move      "&"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-exe-prm-001          .
           move      w-all-str-cat (2)    to   w-exe-prm-002          .
           move      w-all-str-cat (3)    to   w-exe-prm-003          .
           move      w-all-str-cat (4)    to   w-exe-prm-004          .
      *              *-------------------------------------------------*
      *              * Estrazione codice utente                        *
      *              *-------------------------------------------------*
           move      w-exe-prm-001        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-cod-ute          .
      *              *-------------------------------------------------*
      *              * Estrazione password                             *
      *              *-------------------------------------------------*
           move      w-exe-prm-002        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-pwd-ute          .
      *              *-------------------------------------------------*
      *              * Estrazione ID finestra                          *
      *              *-------------------------------------------------*
           move      w-exe-prm-003        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-win-nam          .
      *              *-------------------------------------------------*
      *              * Estrazione status                               *
      *              *-------------------------------------------------*
           move      w-exe-prm-004        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-flg-sts          .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Modulo da eseguire                                        *
      *    *-----------------------------------------------------------*
       exe-mod-000.
       
       
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-sts          .
      *              *-------------------------------------------------*
      *              * Test se sessione gia' attiva                    *
      *              *-------------------------------------------------*
           if        w-exe-win-nam        =    spaces
                     go to exe-mod-020.
      *              *-------------------------------------------------*
      *              * [sss]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
      *              *-------------------------------------------------*
      *              * Lettura record [sss]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "SSSIDX    "         to   f-key                  .
           move      w-exe-win-nam        to   rf-sss-idx-md5         .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
      *              *-------------------------------------------------*
      *              * [sss]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
       exe-mod-020.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-exe-cod-ute        =    spaces
                     move  "U"            to   w-exe-flg-sts
                     go to exe-mod-500.
       exe-mod-030.
      *              *-------------------------------------------------*
      *              * Funzione Open  per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "UTE"                to   j-tre                  .
           move      w-exe-cod-ute        to   j-kre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       exe-mod-050.
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        j-rsc                =    e-not-fnd
                     go to exe-mod-100
           else if   j-rsc                =    spaces
                     go to exe-mod-200
           else      go to exe-mod-300.
       exe-mod-100.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "N"                  to   w-exe-flg-sts          .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-mod-500.
       exe-mod-200.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-exe-flg-sts          .
      *                  *---------------------------------------------*
      *                  * Record in area di lavoro                    *
      *                  *---------------------------------------------*
           move      j-dat                to   w-ute                  .
      *                  *---------------------------------------------*
      *                  * Test su password                            *
      *                  *---------------------------------------------*
           if        w-exe-pwd-ute        not  = spaces        and
                     w-exe-pwd-ute        not  = w-ute-pwd-ute
                     move  "D"            to   w-exe-flg-sts
                     go to exe-mod-500.
           if        w-exe-pwd-ute        = spaces             and
                     w-exe-pwd-ute        not  = w-ute-pwd-ute
                     move  "D"            to   w-exe-flg-sts
                     go to exe-mod-500.
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-mod-500.
       exe-mod-300.
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "E"                  to   w-exe-flg-sts          .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-mod-500.
       exe-mod-500.
       
       
       
       
       
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
                     go to exe-mod-550.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "maucmf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/maucmf"                         .
       
       
       exe-mod-550.
      *              *-------------------------------------------------*
      *              * Test su flag                                    *
      *              *-------------------------------------------------*













       exe-mod-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
