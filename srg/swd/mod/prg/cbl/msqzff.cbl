       Identification Division.
       Program-Id.                                 msqzff             .
      *================================================================*
      *                                                                *
      * Modulo di filtro per operazioni alla prima Open di un File     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Open modulo                                        *
      *                                                                *
      *             Input  : x-ope = "OP"                              *
      *                                                                *
      *                      x-tpf = tipo filtro                       *
      *                              - 0 : nessun filtro               *
      *                              - 1 : filtro standard             *
      *                              - 2 : filtro programma            *
      *                                                                *
      *                      x-nmf = filtro                            *
      *                              - se 0 : Spaces                   *
      *                              - se 1 : Nome filtro standard     *
      *                              - se 2 : Pathname oggetto         *
      *                                                                *
      *             Output : x-sts = Status                            *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Filtering   Esecuzione funzione di filtro sul record           *
      *                                                                *
      *             Input  : x-ope = "FF"                              *
      *                                                                *
      *                      x-rec = record da filtrare                *
      *                                                                *
      *             Output : x-sts = Status                            *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *                      x-rec = record filtrato                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Close modulo                                       *
      *                                                                *
      *             Input  : x-ope = "CL"                              *
      *                                                                *
      *             Output : x-sts = Status                            *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work area per il controllo                                *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Salvataggio tipo di filtro richiesto                  *
      *        * - 0 : nessun filtro                                   *
      *        * - 1 : filtro standard                                 *
      *        * - 2 : filtro programma                                *
      *        *-------------------------------------------------------*
           05  w-cnt-tip-flt              pic  9(01) value zero       .
      *        *-------------------------------------------------------*
      *        * Salvataggio nome filtro o pathname programma oggetto  *
      *        * di filtro                                             *
      *        *-------------------------------------------------------*
           05  w-cnt-nop-flt              pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Area per filtro standard                              *
      *        *-------------------------------------------------------*
           05  w-cnt-std-flt.
      *            *---------------------------------------------------*
      *            * Numero parametri di conversione per il filtro     *
      *            *---------------------------------------------------*
               10  w-cnt-std-num-prm      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Area parametri di conversione per il filtro       *
      *            *---------------------------------------------------*
               10  w-cnt-std-are-prm occurs 36.
      *                *-----------------------------------------------*
      *                * Tipo operazione standard                      *
      *                * - 01 : Normalizzazione                        *
      *                * - 02 : Inserimento campo normalizzato         *
      *                * - 03 : Soppressione campo                     *
      *                * - 04 : Uppercase                              *
      *                * - 05 : Lowercase                              *
      *                *-----------------------------------------------*
                   15  w-cnt-std-tip-ope  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Tipo campo                                    *
      *                * - A : Alfanumerico                            *
      *                * - N : Numerico                                *
      *                *-----------------------------------------------*
                   15  w-cnt-std-tip-cmp  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Numero caratteri                              *
      *                *-----------------------------------------------*
                   15  w-cnt-std-car-cmp  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Numero decimali : 0..5                        *
      *                *-----------------------------------------------*
                   15  w-cnt-std-dec-cmp  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Presenza segno algebrico                      *
      *                * - Spaces : No                                 *
      *                * - S      : Si                                 *
      *                *-----------------------------------------------*
                   15  w-cnt-std-sgn-cmp  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Tipo rappresentazione segno algebrico         *
      *                * - Spaces : Trailing separate character        *
      *                * - E      : Trailing embedded character        *
      *                *-----------------------------------------------*
                   15  w-cnt-std-trs-cmp  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Displacement del campo, partendo da 0001      *
      *                *-----------------------------------------------*
                   15  w-cnt-std-dsp-cmp  pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Valore numerico o alfanumerico                *
      *                *-----------------------------------------------*
                   15  w-cnt-std-val-cmp  pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Indice di scansione su parametri di conversione   *
      *            * per il filtro                                     *
      *            *---------------------------------------------------*
               10  w-cnt-std-inx-prm      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Work-area per record da filtrare                  *
      *            *---------------------------------------------------*
               10  w-cnt-std-wrk-rec.
                   15  w-cnt-std-wrk-chr
                             occurs 5120  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Displacement limite min per funzioni Uppercase e  *
      *            * Lowercase                                         *
      *            *---------------------------------------------------*
               10  w-cnt-std-lds-min      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Displacement limite max per funzioni Uppercase e  *
      *            * Lowercase                                         *
      *            *---------------------------------------------------*
               10  w-cnt-std-lds-max      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Indice per terminazione con high-value            *
      *            *---------------------------------------------------*
               10  w-cnt-std-inx-hva      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Pointer per istruzione 'string'                   *
      *            *---------------------------------------------------*
               10  w-cnt-std-pnt-str      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Pointer per istruzione 'unstring'                 *
      *            *---------------------------------------------------*
               10  w-cnt-std-pnt-uns      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Comodo per calcolo numero bytes totali occupati   *
      *            * da un campo                                       *
      *            *---------------------------------------------------*
               10  w-cnt-std-nbt-odc      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Comodo per calcolo numero bytes totali occupati   *
      *            * da un campo numerico                              *
      *            *---------------------------------------------------*
               10  w-cnt-std-nbt-ocn      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo per Spaces terminati da high-value *
      *            *---------------------------------------------------*
               10  w-cnt-std-spc-all.
                   15  w-cnt-std-spc-chr
                              occurs 1000 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Campo numerico senza segno di 18 cifre a zero     *
      *            *---------------------------------------------------*
               10  w-cnt-std-n18-g01.
                   15  w-cnt-std-n18-z01  pic  9(18)                  .
      *            *---------------------------------------------------*
      *            * Campo numerico con segno trailing separate cha-   *
      *            * racter di 18 cifre a zero                         *
      *            *---------------------------------------------------*
               10  w-cnt-std-n18-g02.
                   15  w-cnt-std-n18-z02  pic s9(18)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *            *---------------------------------------------------*
      *            * Campo numerico con segno trailing embedded cha-   *
      *            * racter di 18 cifre a zero                         *
      *            *---------------------------------------------------*
               10  w-cnt-std-n18-g03.
                   15  w-cnt-std-n18-z03  pic s9(18)       sign is
                                                           trailing   .
      *            *---------------------------------------------------*
      *            * Area di comodo per Zeroes terminati da high-value *
      *            *---------------------------------------------------*
               10  w-cnt-std-zer-all.
                   15  w-cnt-std-zer-chr
                              occurs 20   pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo di transito per l'intero record    *
      *            * per funzioni di inserimento e soppressione campo  *
      *            *---------------------------------------------------*
               10  w-cnt-std-wre-ios.
                   15  w-cnt-std-wre-chr
                             occurs 5120  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase/lowercase       *
      *    *-----------------------------------------------------------*
       01  w-upp-low.
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upp-chr occurs 26    pic  x(01)                  .
           05  w-low-car.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-low-crr redefines w-low-car.
               10  w-low-chr occurs 26    pic  x(01)                  .
           05  w-upp-ctr                  pic  9(02)                  .

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
      *    * Work per records di [auc] 'fcs'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucfcs0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msqzff" *
      *    *-----------------------------------------------------------*
       01  x.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        * - OP : Open                                           *
      *        * - FF : Filtering                                      *
      *        * - CL : Close                                          *
      *        *-------------------------------------------------------*
           05  x-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di filtro richiesto                              *
      *        * - 0 : Nessun filtro                                   *
      *        * - 1 : Filtro standard                                 *
      *        * - 2 : Filtro programma                                *
      *        *-------------------------------------------------------*
           05  x-tpf                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome filtro o pathname programma oggetto di filtro    *
      *        *-------------------------------------------------------*
           05  x-nmf                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Status di uscita                                      *
      *        *-------------------------------------------------------*
           05  x-sts                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Area record da filtrare e filtrato                    *
      *        *-------------------------------------------------------*
           05  x-rec.
               10  x-chr      occurs 5120 pic  x(01)                  .

      ******************************************************************
       Procedure Division                using x                      .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        x-ope                =    "OP"
                     perform opn-opn-000  thru opn-opn-999
           else if   x-ope                =    "FF"
                     perform flt-flt-000  thru flt-flt-999
           else if   x-ope                =    "CL"
                     perform cls-cls-000  thru cls-cls-999            .
       main-999.
           exit      program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-opn-000.
      *              *-------------------------------------------------*
      *              * Salvataggi                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio tipo di filtro                  *
      *                  *---------------------------------------------*
           move      x-tpf                to   w-cnt-tip-flt          .
      *                  *---------------------------------------------*
      *                  * Salvataggio nome filtro o pathname program- *
      *                  * ma oggetto di filtro                        *
      *                  *---------------------------------------------*
           move      x-nmf                to   w-cnt-nop-flt          .
       opn-opn-100.
      *              *-------------------------------------------------*
      *              * Controlli e normalizzazioni                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di filtro                              *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        not  = 1 and
                     w-cnt-tip-flt        not  = 2
                     move  zero           to   w-cnt-tip-flt          .
           if        w-cnt-nop-flt        =    spaces
                     move  zero           to   w-cnt-tip-flt          .
      *                  *---------------------------------------------*
      *                  * Nome o pathname filtro                      *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     move  spaces         to   w-cnt-nop-flt          .
       opn-opn-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di filtro         *
      *              *-------------------------------------------------*
           if        w-cnt-tip-flt        =    1
                     go to opn-opn-400
           else if   w-cnt-tip-flt        =    2
                     go to opn-opn-700.
       opn-opn-300.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 0 : Nessun filtro                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, uscita                      *
      *                  *---------------------------------------------*
           go to     opn-opn-999.
       opn-opn-400.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 1 : Filtro standard              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione della routine di caricamento dei *
      *                  * parametri del filtro standard               *
      *                  *---------------------------------------------*
           perform   std-loa-000          thru std-loa-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-opn-999.
       opn-opn-700.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 2 : Filtro programma             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del programma oggetto di filtro    *
      *                  * per la funzione di Open                     *
      *                  *---------------------------------------------*
           move      "OP"                 to   x-ope                  .
           call      w-cnt-nop-flt       using x                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-opn-999.
       opn-opn-999.
           exit.

      *    *===========================================================*
      *    * Filtering                                                 *
      *    *-----------------------------------------------------------*
       flt-flt-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di filtro         *
      *              *-------------------------------------------------*
           if        w-cnt-tip-flt        =    1
                     go to flt-flt-400
           else if   w-cnt-tip-flt        =    2
                     go to flt-flt-700.
       flt-flt-300.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 0 : Nessun filtro                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, uscita                      *
      *                  *---------------------------------------------*
           go to     flt-flt-999.
       flt-flt-400.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 1 : Filtro standard              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione della routine di filtro standard *
      *                  * su record contenuto in x-rec                *
      *                  *---------------------------------------------*
           perform   std-flt-000          thru std-flt-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     flt-flt-999.
       flt-flt-700.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 2 : Filtro programma             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del programma oggetto di filtro    *
      *                  * per la funzione di Filtering                *
      *                  *---------------------------------------------*
           move      "FF"                 to   x-ope                  .
           call      w-cnt-nop-flt       using x                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cls-cls-999.
       flt-flt-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-cls-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di filtro         *
      *              *-------------------------------------------------*
           if        w-cnt-tip-flt        =    1
                     go to cls-cls-400
           else if   w-cnt-tip-flt        =    2
                     go to cls-cls-700.
       cls-cls-300.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 0 : Nessun filtro                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, uscita                      *
      *                  *---------------------------------------------*
           go to     cls-cls-999.
       cls-cls-400.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 1 : Filtro standard              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, uscita                      *
      *                  *---------------------------------------------*
           go to     cls-cls-999.
       cls-cls-700.
      *              *-------------------------------------------------*
      *              * Se tipo filtro 2 : Filtro programma             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del programma oggetto di filtro    *
      *                  * per la funzione di Close                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   x-ope                  .
           call      w-cnt-nop-flt       using x                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione del programma oggetto di fil- *
      *                  * tro                                         *
      *                  *---------------------------------------------*
           cancel    w-cnt-nop-flt                                    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cls-cls-999.
       cls-cls-999.
           exit.

      *    *===========================================================*
      *    * Routine di caricamento dei parametri del filtro standard  *
      *    * il cui nome e' contenuto in 'w-cnt-nop-flt'               *
      *    *-----------------------------------------------------------*
       std-loa-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero parametri di conversio- *
      *              * ne per il filtro caricati                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-std-num-prm      .
       std-loa-100.
      *              *-------------------------------------------------*
      *              * Funzione Open  per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       std-loa-200.
      *              *-------------------------------------------------*
      *              * Lettura filtro per mezzo del modulo    "maucmf" *
      *              *-------------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "FCS"                to   j-tre                  .
           move      w-cnt-nop-flt        to   j-kre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *              *-------------------------------------------------*
      *              * Se errori in lettura : a chiusura               *
      *              *-------------------------------------------------*
           if        j-rsc                not  = spaces
                     go to std-loa-800.
      *              *-------------------------------------------------*
      *              * Record letto in comodo di ridefinizione         *
      *              *-------------------------------------------------*
           move      j-dat                to   w-fcs                  .
       std-loa-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record letto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indice di scansione su record originale a   *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-cnt-std-inx-prm      .
       std-loa-400.
      *                  *---------------------------------------------*
      *                  * Incremento indice di scansione su record    *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-std-inx-prm      .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : fine bufferizzazione  *
      *                  *---------------------------------------------*
           if        w-cnt-std-inx-prm    >    36
                     go to std-loa-800.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione a Spaces : si ignora     *
      *                  * l'elemento in esame                         *
      *                  *---------------------------------------------*
           if        w-fcs-tip-ope
                    (w-cnt-std-inx-prm)   =    spaces
                     go to std-loa-400.
      *                  *---------------------------------------------*
      *                  * Incremento numero parametri effettivi       *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-std-num-prm      .
      *                  *---------------------------------------------*
      *                  * Spostamento parametri da record originale a *
      *                  * buffer interno                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione                         *
      *                      *-----------------------------------------*
           move      w-fcs-tip-ope
                    (w-cnt-std-inx-prm)   to   w-cnt-std-tip-ope
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Tipo campo                              *
      *                      *-----------------------------------------*
           move      w-fcs-tip-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-tip-cmp
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Numero caratteri                        *
      *                      *-----------------------------------------*
           move      w-fcs-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-car-cmp
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Numero decimali                         *
      *                      *-----------------------------------------*
           move      w-fcs-dec-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-dec-cmp
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Presenza segno algebrico                *
      *                      *-----------------------------------------*
           move      w-fcs-sgn-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-sgn-cmp
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Tipo rappresentazione segno algebrico   *
      *                      *-----------------------------------------*
           move      w-fcs-trs-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-trs-cmp
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Displacement per il campo               *
      *                      *-----------------------------------------*
           move      w-fcs-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-dsp-cmp
                                              (w-cnt-std-num-prm)     .
      *                      *-----------------------------------------*
      *                      * Valore per il campo                     *
      *                      *-----------------------------------------*
           move      w-fcs-val-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-val-cmp
                                              (w-cnt-std-num-prm)     .
      *                  *---------------------------------------------*
      *                  * Riciclo ad elemento successivo              *
      *                  *---------------------------------------------*
           go to     std-loa-400.
       std-loa-800.
      *              *-------------------------------------------------*
      *              * Funzione Close per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       std-loa-900.
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "maucmf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
           if        j-rsc                not  = spaces
                     go to std-loa-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "maucmf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/maucmf"                         .
       std-loa-999.
           exit.

      *    *===========================================================*
      *    * Routine di esecuzione di filtro standard sul record con-  *
      *    * tenuto in 'x-rec', in base ai parametri caricati in area  *
      *    * 'w-cnt-std-flt'.                                          *
      *    *-----------------------------------------------------------*
       std-flt-000.
      *              *-------------------------------------------------*
      *              * Se numero parametri di conversione per il fil-  *
      *              * tro pari a zero : uscita senza alcuna azione    *
      *              *-------------------------------------------------*
           if        w-cnt-std-num-prm    =    zero
                     go to std-flt-999.
      *              *-------------------------------------------------*
      *              * Spostamento record da link-area ad area di work *
      *              *-------------------------------------------------*
           move      x-rec                to   w-cnt-std-wrk-rec      .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice di scansione su parame- *
      *              * tri di conversione per il filtro                *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-std-inx-prm      .
       std-flt-100.
      *              *-------------------------------------------------*
      *              * Incremento indice di scansione su parametri di  *
      *              * conversione per il filtro                       *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-std-inx-prm      .
      *              *-------------------------------------------------*
      *              * Se oltre numero di parametri di conversione per *
      *              * il filtro : ad uscita                           *
      *              *-------------------------------------------------*
           if        w-cnt-std-inx-prm    >    w-cnt-std-num-prm
                     go to std-flt-900.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cnt-std-tip-ope
                    (w-cnt-std-inx-prm)   =    01
                     go to std-flt-200
           else if   w-cnt-std-tip-ope
                    (w-cnt-std-inx-prm)   =    02
                     go to std-flt-300
           else if   w-cnt-std-tip-ope
                    (w-cnt-std-inx-prm)   =    03
                     go to std-flt-400
           else if   w-cnt-std-tip-ope
                    (w-cnt-std-inx-prm)   =    04
                     go to std-flt-500
           else if   w-cnt-std-tip-ope
                    (w-cnt-std-inx-prm)   =    05
                     go to std-flt-600.
      *              *-------------------------------------------------*
      *              * Se tipo operazione non riconosciuto : si ignora *
      *              * il parametro di conversione e si ricicla sul    *
      *              * parametro di conversione successivo             *
      *              *-------------------------------------------------*
           go to     std-flt-100.
       std-flt-200.
      *              *-------------------------------------------------*
      *              * Tipo operazione 01 : Normalizzazione            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di Normalizzazione                  *
      *                  *---------------------------------------------*
           perform   std-flt-nor-000      thru std-flt-nor-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a prossimo parametro di conversione *
      *                  *---------------------------------------------*
           go to     std-flt-100.
       std-flt-300.
      *              *-------------------------------------------------*
      *              * Tipo operazione 02 : Inserimento di un campo    *
      *              *                      normalizzato               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di Inserimento                      *
      *                  *---------------------------------------------*
           perform   std-flt-ins-000      thru std-flt-ins-999        .
      *                  *---------------------------------------------*
      *                  * Routine di Normalizzazione                  *
      *                  *---------------------------------------------*
           perform   std-flt-nor-000      thru std-flt-nor-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a prossimo parametro di conversione *
      *                  *---------------------------------------------*
           go to     std-flt-100.
       std-flt-400.
      *              *-------------------------------------------------*
      *              * Tipo operazione 02 : Soppressione di un campo   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di Soppressione                     *
      *                  *---------------------------------------------*
           perform   std-flt-sop-000      thru std-flt-sop-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a prossimo parametro di conversione *
      *                  *---------------------------------------------*
           go to     std-flt-100.
       std-flt-500.
      *              *-------------------------------------------------*
      *              * Tipo operazione 04 : Uppercase                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di Uppercase                        *
      *                  *---------------------------------------------*
           perform   std-flt-upc-000      thru std-flt-upc-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a prossimo parametro di conversione *
      *                  *---------------------------------------------*
           go to     std-flt-100.
       std-flt-600.
      *              *-------------------------------------------------*
      *              * Tipo operazione 05 : Lowercase                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di Lowercase                        *
      *                  *---------------------------------------------*
           perform   std-flt-loc-000      thru std-flt-loc-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a prossimo parametro di conversione *
      *                  *---------------------------------------------*
           go to     std-flt-100.
       std-flt-900.
      *              *-------------------------------------------------*
      *              * Spostamento record da area di work a link-area  *
      *              *-------------------------------------------------*
           move      w-cnt-std-wrk-rec    to   x-rec                  .
       std-flt-999.
           exit.

      *    *===========================================================*
      *    * Filtro standard : Normalizzazione                         *
      *    *-----------------------------------------------------------*
       std-flt-nor-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo campo           *
      *              *-------------------------------------------------*
           if        w-cnt-std-tip-cmp
                    (w-cnt-std-inx-prm)   =    "A"
                     go to std-flt-nor-100
           else if   w-cnt-std-tip-cmp
                    (w-cnt-std-inx-prm)   =    "N"
                     go to std-flt-nor-200.
       std-flt-nor-100.
      *              *-------------------------------------------------*
      *              * Se tipo campo 'A' : Alfanumerico                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione spaces terminati da high-value *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-std-spc-all      .
           move      w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-inx-hva      .
           add       1                    to   w-cnt-std-inx-hva      .
           move      high-value           to   w-cnt-std-spc-chr
                                              (w-cnt-std-inx-hva)     .
      *                  *---------------------------------------------*
      *                  * Preparazione pointer per 'string'           *
      *                  *---------------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-str      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione effettiva                   *
      *                  *---------------------------------------------*
           string    w-cnt-std-spc-all
                                delimited by   high-value
                                          into w-cnt-std-wrk-rec
                                  with pointer w-cnt-std-pnt-str      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     std-flt-nor-999.
       std-flt-nor-200.
      *              *-------------------------------------------------*
      *              * Se tipo campo 'N' : Numerico                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il campo ha segno   *
      *                  * algebrico oppure no                         *
      *                  *---------------------------------------------*
           if        w-cnt-std-sgn-cmp
                    (w-cnt-std-inx-prm)   =    "S"
                     go to std-flt-nor-400.
       std-flt-nor-300.
      *                  *---------------------------------------------*
      *                  * Se campo senza segno algebrico              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore numerico max a zero              *
      *                      *-----------------------------------------*
           move      zero                 to   w-cnt-std-n18-z01      .
      *                      *-----------------------------------------*
      *                      * Preparazione zeroes terminati da high-  *
      *                      * value                                   *
      *                      *-----------------------------------------*
           move      w-cnt-std-n18-g01    to   w-cnt-std-zer-all      .
           move      w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-inx-hva      .
           add       w-cnt-std-dec-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-inx-hva      .
           add       1                    to   w-cnt-std-inx-hva      .
           move      high-value           to   w-cnt-std-zer-chr
                                              (w-cnt-std-inx-hva)     .
      *                      *-----------------------------------------*
      *                      * Preparazione pointer per 'string'       *
      *                      *-----------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-str      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione effettiva               *
      *                      *-----------------------------------------*
           string    w-cnt-std-zer-all
                                delimited by   high-value
                                          into w-cnt-std-wrk-rec
                                  with pointer w-cnt-std-pnt-str      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     std-flt-nor-999.
       std-flt-nor-400.
      *                  *---------------------------------------------*
      *                  * Se campo con segno algebrico                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di rap-   *
      *                      * presentazione del segno                 *
      *                      *-----------------------------------------*
           if        w-cnt-std-trs-cmp
                    (w-cnt-std-inx-prm)   =    "E"
                     go to std-flt-nor-600.
       std-flt-nor-500.
      *                      *-----------------------------------------*
      *                      * Se Trailing separate character          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore numerico max a zero          *
      *                          *-------------------------------------*
           move      zero                 to   w-cnt-std-n18-z02      .
      *                          *-------------------------------------*
      *                          * Calcolo numero bytes totali occupa- *
      *                          * ti dal campo numerico               *
      *                          *-------------------------------------*
           move      w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-ocn      .
           add       w-cnt-std-dec-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-ocn      .
           add       1                    to   w-cnt-std-nbt-ocn      .
      *                          *-------------------------------------*
      *                          * Calcolo pointer per unstring        *
      *                          *-------------------------------------*
           move      20                   to   w-cnt-std-pnt-uns      .
           subtract  w-cnt-std-nbt-ocn    from w-cnt-std-pnt-uns      .
      *                          *-------------------------------------*
      *                          * Spostamento valore a zero in area   *
      *                          * di comodo con allineamento a sini-  *
      *                          * stra                                *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-std-zer-all      .
           unstring  w-cnt-std-n18-g02    into w-cnt-std-zer-all
                                  with pointer w-cnt-std-pnt-uns      .
      *                          *-------------------------------------*
      *                          * Terminazione area di comodo con ca- *
      *                          * rattere di high-value               *
      *                          *-------------------------------------*
           move      w-cnt-std-nbt-ocn    to   w-cnt-std-inx-hva      .
           add       1                    to   w-cnt-std-inx-hva      .
           move      high-value           to   w-cnt-std-zer-chr
                                              (w-cnt-std-inx-hva)     .
      *                          *-------------------------------------*
      *                          * Preparazione pointer 'string'       *
      *                          *-------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-str      .
      *                          *-------------------------------------*
      *                          * Normalizzazione effettiva           *
      *                          *-------------------------------------*
           string    w-cnt-std-zer-all
                                delimited by   high-value
                                          into w-cnt-std-wrk-rec
                                  with pointer w-cnt-std-pnt-str      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     std-flt-nor-999.
       std-flt-nor-600.
      *                      *-----------------------------------------*
      *                      * Se Trailing embedded character          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore numerico max a zero          *
      *                          *-------------------------------------*
           move      zero                 to   w-cnt-std-n18-z03      .
      *                          *-------------------------------------*
      *                          * Calcolo numero bytes totali occupa- *
      *                          * ti dal campo numerico               *
      *                          *-------------------------------------*
           move      w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-ocn      .
           add       w-cnt-std-dec-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-ocn      .
      *                          *-------------------------------------*
      *                          * Calcolo pointer per unstring        *
      *                          *-------------------------------------*
           move      19                   to   w-cnt-std-pnt-uns      .
           subtract  w-cnt-std-nbt-ocn    from w-cnt-std-pnt-uns      .
      *                          *-------------------------------------*
      *                          * Spostamento valore a zero in area   *
      *                          * di comodo con allineamento a sini-  *
      *                          * stra                                *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-std-zer-all      .
           unstring  w-cnt-std-n18-g03    into w-cnt-std-zer-all
                                  with pointer w-cnt-std-pnt-uns      .
      *                          *-------------------------------------*
      *                          * Terminazione area di comodo con ca- *
      *                          * rattere di high-value               *
      *                          *-------------------------------------*
           move      w-cnt-std-nbt-ocn    to   w-cnt-std-inx-hva      .
           add       1                    to   w-cnt-std-inx-hva      .
           move      high-value           to   w-cnt-std-zer-chr
                                              (w-cnt-std-inx-hva)     .
      *                          *-------------------------------------*
      *                          * Preparazione pointer 'string'       *
      *                          *-------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-str      .
      *                          *-------------------------------------*
      *                          * Normalizzazione effettiva           *
      *                          *-------------------------------------*
           string    w-cnt-std-zer-all
                                delimited by   high-value
                                          into w-cnt-std-wrk-rec
                                  with pointer w-cnt-std-pnt-str      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     std-flt-nor-999.
       std-flt-nor-999.
           exit.

      *    *===========================================================*
      *    * Filtro standard : Inserimento                             *
      *    *-----------------------------------------------------------*
       std-flt-ins-000.
      *              *-------------------------------------------------*
      *              * Calcolo numero totale di bytes occupati dal     *
      *              * campo                                           *
      *              *-------------------------------------------------*
           move      w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-odc      .
           if        w-cnt-std-tip-cmp
                    (w-cnt-std-inx-prm)   not  = "N"
                     go to std-flt-ins-100.
           add       w-cnt-std-dec-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-odc      .
           if        w-cnt-std-sgn-cmp
                    (w-cnt-std-inx-prm)   not  = "S"
                     go to std-flt-ins-100.
           if        w-cnt-std-trs-cmp
                    (w-cnt-std-inx-prm)   =    "E"
                     go to std-flt-ins-100.
           add       1                    to   w-cnt-std-nbt-odc      .
       std-flt-ins-100.
      *              *-------------------------------------------------*
      *              * Area corrispondente al punto di inserimento in  *
      *              * comodo di transito                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-std-wre-ios      .
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-uns      .
           unstring  w-cnt-std-wrk-rec    into w-cnt-std-wre-ios
                                  with pointer w-cnt-std-pnt-uns      .
      *              *-------------------------------------------------*
      *              * Area di comodo di transito in area successiva   *
      *              * al punto di inserimento                         *
      *              *-------------------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-str      .
           add       w-cnt-std-nbt-odc    to   w-cnt-std-pnt-str      .
           string    w-cnt-std-wre-ios
                                delimited by   size
                                          into w-cnt-std-wrk-rec
                                  with pointer w-cnt-std-pnt-str      .
       std-flt-ins-999.
           exit.

      *    *===========================================================*
      *    * Filtro standard : Soppressione                            *
      *    *-----------------------------------------------------------*
       std-flt-sop-000.
      *              *-------------------------------------------------*
      *              * Calcolo numero totale di bytes occupati dal     *
      *              * campo                                           *
      *              *-------------------------------------------------*
           move      w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-odc      .
           if        w-cnt-std-tip-cmp
                    (w-cnt-std-inx-prm)   not  = "N"
                     go to std-flt-sop-100.
           add       w-cnt-std-dec-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-nbt-odc      .
           if        w-cnt-std-sgn-cmp
                    (w-cnt-std-inx-prm)   not  = "S"
                     go to std-flt-sop-100.
           if        w-cnt-std-trs-cmp
                    (w-cnt-std-inx-prm)   =    "E"
                     go to std-flt-sop-100.
           add       1                    to   w-cnt-std-nbt-odc      .
       std-flt-sop-100.
      *              *-------------------------------------------------*
      *              * Area successiva al punto di inserimento in co-  *
      *              * modo di transito                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-std-wre-ios      .
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-uns      .
           add       w-cnt-std-nbt-odc    to   w-cnt-std-pnt-uns      .
           unstring  w-cnt-std-wrk-rec    into w-cnt-std-wre-ios
                                  with pointer w-cnt-std-pnt-uns      .
      *              *-------------------------------------------------*
      *              * Area di comodo di transito in area corrispon-   *
      *              * dente al punto di inserimento                   *
      *              *-------------------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-pnt-str      .
           string    w-cnt-std-wre-ios
                                delimited by   size
                                          into w-cnt-std-wrk-rec
                                  with pointer w-cnt-std-pnt-str      .
       std-flt-sop-999.
           exit.

      *    *===========================================================*
      *    * Filtro standard : Uppercase                               *
      *    *-----------------------------------------------------------*
       std-flt-upc-000.
      *              *-------------------------------------------------*
      *              * Preparazione displacement limite min            *
      *              *-------------------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-lds-min      .
      *              *-------------------------------------------------*
      *              * Preparazione displacement limite max            *
      *              *-------------------------------------------------*
           move      w-cnt-std-lds-min    to   w-cnt-std-lds-max      .
           add       w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-lds-max      .
           subtract  1                    from w-cnt-std-lds-max      .
       std-flt-upc-200.
      *              *-------------------------------------------------*
      *              * Funzione di Uppercase vera e propria            *
      *              *-------------------------------------------------*
       std-flt-upc-300.
           move      zero                 to   w-upp-ctr              .
           inspect   w-low-car        tallying w-upp-ctr
                     for characters     before
                                       initial w-cnt-std-wrk-chr
                                              (w-cnt-std-lds-max)     .
           if        w-upp-ctr            not  < 26
                     go to std-flt-upc-400.
           add       1                    to   w-upp-ctr              .
           move      w-upp-chr
                    (w-upp-ctr)           to   w-cnt-std-wrk-chr
                                              (w-cnt-std-lds-max)     .
       std-flt-upc-400.
           subtract  1                    from w-cnt-std-lds-max      .
           if        w-cnt-std-lds-max    not  < w-cnt-std-lds-min
                     go to std-flt-upc-300.
       std-flt-upc-999.
           exit.

      *    *===========================================================*
      *    * Filtro standard : Lowercase                               *
      *    *-----------------------------------------------------------*
       std-flt-loc-000.
      *              *-------------------------------------------------*
      *              * Preparazione displacement limite min            *
      *              *-------------------------------------------------*
           move      w-cnt-std-dsp-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-lds-min      .
      *              *-------------------------------------------------*
      *              * Preparazione displacement limite max            *
      *              *-------------------------------------------------*
           move      w-cnt-std-lds-min    to   w-cnt-std-lds-max      .
           add       w-cnt-std-car-cmp
                    (w-cnt-std-inx-prm)   to   w-cnt-std-lds-max      .
           subtract  1                    from w-cnt-std-lds-max      .
       std-flt-loc-200.
      *              *-------------------------------------------------*
      *              * Funzione di Lowercase vera e propria            *
      *              *-------------------------------------------------*
       std-flt-loc-300.
           move      zero                 to   w-upp-ctr              .
           inspect   w-upp-car        tallying w-upp-ctr
                     for characters     before
                                       initial w-cnt-std-wrk-chr
                                              (w-cnt-std-lds-max)     .
           if        w-upp-ctr            not  < 26
                     go to std-flt-loc-400.
           add       1                    to   w-upp-ctr              .
           move      w-low-chr
                    (w-upp-ctr)           to   w-cnt-std-wrk-chr
                                              (w-cnt-std-lds-max)     .
       std-flt-loc-400.
           subtract  1                    from w-cnt-std-lds-max      .
           if        w-cnt-std-lds-max    not  < w-cnt-std-lds-min
                     go to std-flt-loc-300.
       std-flt-loc-999.
           exit.

