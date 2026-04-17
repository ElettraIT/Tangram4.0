       Identification Division.
       Program-Id.                                 dirdacq0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:    doc                 *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/04/25    *
      *                       Ultima revisione:    NdK del 23/04/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione delle inter-relazioni tra i       *
      * documenti di vendita                                           *
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
      *        Input  : d-ird-acq-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-ird-acq-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-ird-acq-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-ird-acq-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "D<" - Determinazione correlazioni documento, gerarchia        *
      *        decrescente                                             *
      *                                                                *
      *       (fattura, bolla, spedizione, ordine)                     *
      *                                                                *
      *                                                                *
      *        Input  : d-ird-acq-tip-ope = "D<"                       *
      *                                                                *
      *                 d-ird-acq-tip-dsr = Tipo documento             *
      *                                                                *
      *                 d-ird-acq-prt-dsr = Protocollo documento       *
      *                                                                *
      *                 d-ird-acq-prg-dsr = Progressivo documento      *
      *                                    (facoltativo)               *
      *                                                                *
      *                                                                *
      *        Output : d-ird-acq-buf-ele = Buffer documenti correlati *
      *                                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "D>" - Determinazione correlazioni documento, gerarchia        *
      *        crescente                                               *
      *                                                                *
      *       (ordine, spedizione, bolla, fattura                      *
      *                                                                *
      *                                                                *
      *        Input  : d-ird-acq-tip-ope = "D>"                       *
      *                                                                *
      *                 d-ird-acq-tip-dsr = Tipo documento             *
      *                                                                *
      *                 d-ird-acq-prt-dsr = Protocollo documento       *
      *                                                                *
      *                 d-ird-acq-prg-dsr = Progressivo documento      *
      *                                    (facoltativo)               *
      *                                                                *
      *                                                                *
      *        Output : d-ird-acq-buf-ele = Buffer documenti correlati *
      *                                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "X<" - Determinazione correlazioni documento, gerarchia        *
      *        decrescente, su file XML                                *
      *                                                                *
      *        ____ DA IMPLEMENTARE ____                               *
      *                                                                *
      *                                                                *
      *                                                                *
      * <documenti>
      *     <fat id="419" tipo="Fattura" url="ele/doc/fat/2025/25_000419.pdf" data="31/01/25">
      *         <bol id="1200" tipo="Bolla" url="ele/doc/bol/2025/25_001200.pdf" data="23/01/25">
      *             <ods id="1400" tipo="Ordine di spedizione" url="ele/doc/ods/2025/25_001400.pdf" data="23/01/23">
      *                 <orc id="981" tipo="Ordine Cliente" url="ele/doc/orc/2025/25_000981.pdf" data="23/01/23"/>
      *             </ods>
      *         </bol>
      *     </fat>
      * </documenti>
      *                                                                *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ida]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfida"                          .
      *        *-------------------------------------------------------*
      *        * [fft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rffft"                          .
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .

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
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
           05  w-wrk-num-doc              pic  9(11)                  .
           05  w-wrk-num-doc-r            redefines
               w-wrk-num-doc                                          .
               10  w-wrk-ndo-saa          pic  9(03)                  .
               10  w-wrk-ndo-dpz          pic  9(02)                  .
               10  w-wrk-ndo-prg          pic  9(06)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Numero elementi nel buffer                            *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-num          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi nel buffer                    *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-max          pic  9(03) value 300        .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-ctr          pic  9(03)                  .
           05  w-det-int-doc-c01          pic  9(03)                  .
           05  w-det-int-doc-c02          pic  9(03)                  .
           05  w-det-int-doc-c03          pic  9(03)                  .
           05  w-det-int-doc-c04          pic  9(03)                  .
           05  w-det-int-doc-svk.
               10  filler                 pic  9(11)                  .
               10  filler                 pic  9(11)                  .
               10  filler                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Tipo documento per scansione                          *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-tdo          pic  9(02)                  .
           05  w-det-int-doc-let          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Protocollo documento per scansione                    *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-pdo          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Tipo elemento                                         *
      *        *                                                       *
      *        * - '1' : 1. elemento per la chiave                     *
      *        * - '2' : 2. elemento per la chiave                     *
      *        * - '3' : 3. elemento per la chiave                     *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-tek          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi di rottura                                     *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-rot.
               10  w-det-int-doc-rtp      pic  9(02)                  .
               10  w-det-int-doc-rpr      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto di comodo                                 *
      *        *-------------------------------------------------------*
           05  w-det-int-doc-cst occurs    300
                                   indexed by  w-det-int-doc-inx       .
               10  w-det-int-doc-key.
                   15  w-det-int-doc-kal.
                           20  w-det-int-doc-k01
                                          pic  9(11)                  .
                           20  w-det-int-doc-k02
                                          pic  9(11)                  .
                           20  w-det-int-doc-k03
                                          pic  9(11)                  .
               10  w-det-int-doc-tip      pic  9(02)                  .
               10  w-det-int-doc-prt      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det annotazioni                              *
      *        *-------------------------------------------------------*
           05  w-det-ann-doc.
               10  w-det-ann-doc-des      pic  x(27)                  .
               10  w-det-ann-doc-ned      pic  x(06)                  .
               10  w-det-ann-doc-ded      pic  x(08)                  .

      *    *===========================================================*
      *    * Work-area per ripristino Start                            *
      *    *-----------------------------------------------------------*
       01  w-rip-str.
      *        *-------------------------------------------------------*
      *        * Work-area per ripristino Start su [ida]               *
      *        *-------------------------------------------------------*
           05  w-rip-str-fft.
      *            *---------------------------------------------------*
      *            * Flag di End-of-file                               *
      *            *---------------------------------------------------*
               10  w-rip-str-fft-eof      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Parametri di Start                                *
      *            *---------------------------------------------------*
               10  w-rip-str-tip-dsr      pic  9(02)                  .
               10  w-rip-str-prt-dsr      pic  9(11)                  .
               10  w-rip-str-prg-dsr      pic  9(05)                  .
               10  w-rip-str-tip-dtr      pic  9(02)                  .
               10  w-rip-str-prt-dtr      pic  9(11)                  .
               10  w-rip-str-prg-dtr      pic  9(05)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * fornitore                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dstsorf0.dtl"                   .

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
      *    * Area di comunicazione per determinazione inter-relazioni  *
      *    * documenti di vendita                                      *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dirdacq0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-ird-acq              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-ird-acq-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione gerarchia decrescente        *
      *                  * (da Fattura a Ordine)                       *
      *                  *---------------------------------------------*
           if        d-ird-acq-tip-ope    =    "D<"
                     perform det-ger-dec-000
                                          thru det-ger-dec-999
      *                  *---------------------------------------------*
      *                  * Determinazione gerarchia crescente          *
      *                  * (da Ordine a Fattura)                       *
      *                  *---------------------------------------------*
           else if   d-ird-acq-tip-ope    =    "D>"
                     perform det-ger-cre-000
                                          thru det-ger-cre-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-ird-acq-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-ird-acq-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-ird-acq-tip-ope    =    "C?"
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
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * [ida]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofida"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ida                 .
      *              *-------------------------------------------------*
      *              * [fft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [oft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine     *
      *              *-------------------------------------------------*
           perform   det-sts-orf-opn-000  thru det-sts-orf-opn-999    .
       rou-opn-fls-999.
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
      *              * [ida]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofida"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ida                 .
      *              *-------------------------------------------------*
      *              * [fft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [oft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              *-------------------------------------------------*
           perform   det-sts-orf-cls-000  thru det-sts-orf-cls-999    .
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
                     move  spaces         to   d-ird-acq-exi-sts
           else      move  "#"            to   d-ird-acq-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento: gerarchia       *
      *    *                                           decrescente     *
      *    *-----------------------------------------------------------*
       det-ger-dec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-acq-num-ele      .
           move      zero                 to   w-det-int-doc-num      .
       det-ger-dec-050.
      *              *-------------------------------------------------*
      *              * Test preliminare su codici in input             *
      *              *-------------------------------------------------*
           if        d-ird-acq-tip-dsr    =    zero or
                     d-ird-acq-prt-dsr    =    zero
                     go to det-ger-dec-900.
       det-ger-dec-060.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           perform   det-ger-doc-nor-000  thru det-ger-doc-nor-999    .
       det-ger-dec-200.
      *              *-------------------------------------------------*
      *              * I ciclo di scansione : generico                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * determinati per il tipo documento richiesto *
      *                  *---------------------------------------------*
           move      d-ird-acq-tip-dsr    to   w-det-int-doc-tdo      .
           move      d-ird-acq-prt-dsr    to   w-det-int-doc-pdo      .
           move      "1"                  to   w-det-int-doc-tek      .
           perform   det-ger-dec-ida-000  thru det-ger-dec-ida-999    .
      *                  *---------------------------------------------*
      *                  * Test sul numero di elementi trovati         *
      *                  *---------------------------------------------*
           if        w-det-int-doc-num    not  > zero
                     move  zero           to   d-ird-acq-num-ele
                     go to det-ger-dec-900.
       det-ger-dec-250.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo documento sor-  *
      *              * gente                                           *
      *              *-------------------------------------------------*
           if        d-ird-acq-tip-dsr    =    31
                     go to det-ger-dec-300
           else if   d-ird-acq-tip-dsr    =    21
                     go to det-ger-dec-400
           else if   d-ird-acq-tip-dsr    =    11
                     go to det-ger-dec-500.
       det-ger-dec-300.
      *              *-------------------------------------------------*
      *              * II ciclo di scansione : bolle                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * 'bolla' determinati nella scansione         *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-dec-320.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-dec-400.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-dec-400.
           if        w-det-int-doc-prt
                    (w-det-int-doc-ctr)   =    zero
                     go to det-ger-dec-400.
           if        w-det-int-doc-tip
                    (w-det-int-doc-ctr)   not  = 21
                     go to det-ger-dec-320.
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           move      21                   to   w-det-int-doc-tdo      .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   w-det-int-doc-pdo      .
           move      "2"                  to   w-det-int-doc-tek      .
           perform   det-ger-dec-ida-000  thru det-ger-dec-ida-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo a 'bolla' successiva                *
      *                  *---------------------------------------------*
           go to     det-ger-dec-320.
       det-ger-dec-400.
      *              *-------------------------------------------------*
      *              * III ciclo di scansione : Ordini di spedizione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * 'spedizione' determinati nella scansione    *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-dec-420.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-dec-500.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-dec-500.
           if        w-det-int-doc-prt
                    (w-det-int-doc-ctr)   =    zero
                     go to det-ger-dec-500.
           if        w-det-int-doc-tip
                    (w-det-int-doc-ctr)   not  = 11
                     go to det-ger-dec-420.
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           move      11                   to   w-det-int-doc-tdo      .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   w-det-int-doc-pdo      .
           move      "3"                  to   w-det-int-doc-tek      .
           perform   det-ger-dec-ida-000  thru det-ger-dec-ida-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo a 'ods' successiva                  *
      *                  *---------------------------------------------*
           go to     det-ger-dec-420.
       det-ger-dec-500.
      *              *-------------------------------------------------*
      *              * IV ciclo di scansione : Ordini                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * 'ordine' determinati nella scansione        *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-dec-520.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-dec-600.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-dec-600.
           if        w-det-int-doc-prt
                    (w-det-int-doc-ctr)   =    zero
                     go to det-ger-dec-600.
           if        w-det-int-doc-tip
                    (w-det-int-doc-ctr)   not  = 01
                     go to det-ger-dec-520.
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           move      01                   to   w-det-int-doc-tdo      .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   w-det-int-doc-pdo      .
           move      "3"                  to   w-det-int-doc-tek      .
           perform   det-ger-dec-ida-000  thru det-ger-dec-ida-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo a 'orc' successiva                  *
      *                  *---------------------------------------------*
           go to     det-ger-dec-520.
       det-ger-dec-600.
      *              *=================================================*
      *              * Ordinamento tabella                             *
      *              *-------------------------------------------------*
           perform   det-ger-doc-srt-000  thru det-ger-doc-srt-999    .
       det-ger-dec-800.
      *              *=================================================*
      *              * Castelletto determinato in area di output       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione castelletto                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-dec-820.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-dec-900.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-dec-900.
      *                  *---------------------------------------------*
      *                  * Tipo documento sorgente                     *
      *                  *---------------------------------------------*
           move      w-det-int-doc-tip
                    (w-det-int-doc-ctr)   to   d-ird-acq-tip-dtr
                                              (w-det-int-doc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in funzione del tipo di     *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           move      w-det-int-doc-tip
                    (w-det-int-doc-ctr)   to   w-det-int-doc-let      .
           perform   det-ger-let-doc-000  thru det-ger-let-doc-999    .
      *                  *---------------------------------------------*
      *                  * Aggiornamento contatore elementi            *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           add       1                    to   d-ird-acq-num-ele      .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-dec-820.
       det-ger-dec-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-dec-999.
       det-ger-dec-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento: gerarchia       *
      *    *                                           decrescente     *                                               *
      *    *                                                           *
      *    * Subroutine per scansione generica [ida]                   *
      *    *-----------------------------------------------------------*
       det-ger-dec-ida-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per rottura              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-int-doc-rtp      .
           move      zero                 to   w-det-int-doc-rpr      .
       det-ger-dec-ida-100.
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SRGTRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-int-doc-tdo    to   rf-ida-tip-dsr         .
           move      w-det-int-doc-pdo    to   rf-ida-prt-dsr         .
           move      zero                 to   rf-ida-prg-dsr         .
           move      zero                 to   rf-ida-tip-dtr         .
           move      zero                 to   rf-ida-prt-dtr         .
           move      zero                 to   rf-ida-prg-dtr         .
           move      "pgm/ffo/fls/ioc/obj/iofida"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ida                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : ad uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-dec-ida-900.
       det-ger-dec-ida-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [ida]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofida"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ida                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-dec-ida-900.
       det-ger-dec-ida-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : ad uscita                 *
      *              *-------------------------------------------------*
           if        rf-ida-tip-dsr       not  = w-det-int-doc-tdo or
                     rf-ida-prt-dsr       not  = w-det-int-doc-pdo
                     go to det-ger-dec-ida-900.
       det-ger-dec-ida-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-ger-dec-ida-500.
      *              *-------------------------------------------------*
      *              * Test di rottura                                 *
      *              *-------------------------------------------------*
           if        rf-ida-tip-dtr       =    w-det-int-doc-rtp and
                     rf-ida-prt-dtr       =    w-det-int-doc-rpr
                     go to det-ger-dec-ida-200.
       det-ger-dec-ida-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento rottura                           *
      *              *-------------------------------------------------*
           move      rf-ida-tip-dtr       to   w-det-int-doc-rtp      .
           move      rf-ida-prt-dtr       to   w-det-int-doc-rpr      .
       det-ger-dec-ida-650.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento contatore documenti           *
      *                  *---------------------------------------------*
           add       1                    to   w-det-int-doc-num      .
      *                  *---------------------------------------------*
      *                  * Chiavi di ordinamento                       *
      *                  *---------------------------------------------*
           if        w-det-int-doc-tek    =    "1"
                     move  rf-ida-prt-dtr to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "2"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
                     move  rf-ida-prt-dtr to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "3"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
                     move  rf-ida-prt-dtr to   w-det-int-doc-k03
                                              (w-det-int-doc-num)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
           move      rf-ida-tip-dtr       to   w-det-int-doc-tip
                                              (w-det-int-doc-num)     .
           move      rf-ida-prt-dtr       to   w-det-int-doc-prt
                                              (w-det-int-doc-num)     .
       det-ger-dec-ida-700.
      *              *-------------------------------------------------*
      *              * Trattamento elemento di ultimo livello per as-  *
      *              * sociare il protocollo in chiave mancante        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ultimo livello (ordine)             *
      *                  *---------------------------------------------*
           if        w-det-int-doc-tek    not  = "3"
                     go to det-ger-dec-ida-800.
      *                  *---------------------------------------------*
      *                  * Ricerca elemento castelletto protocolli     *
      *                  *---------------------------------------------*
           set       w-det-int-doc-inx    to   1                      .
           search    w-det-int-doc-cst
                     when    w-det-int-doc-tip
                            (w-det-int-doc-inx)
                                          =    w-det-int-doc-tdo and
                             w-det-int-doc-prt
                            (w-det-int-doc-inx)
                                          =    w-det-int-doc-pdo
                     go to   det-ger-dec-ida-720.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-dec-ida-800.
       det-ger-dec-ida-720.
      *                  *---------------------------------------------*
      *                  * Se elemento non trovato: completamento      *
      *                  * chiave di ordinamento                       *
      *                  *---------------------------------------------*
           move      w-det-int-doc-k01
                    (w-det-int-doc-inx)   to   w-det-int-doc-k01
                                              (w-det-int-doc-num)     .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-dec-ida-800.
       det-ger-dec-ida-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ger-dec-ida-200.
       det-ger-dec-ida-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-dec-ida-999.
       det-ger-dec-ida-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento: gerarchia       *
      *    *                                           crescente       *
      *    *-----------------------------------------------------------*
       det-ger-cre-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-acq-num-ele      .
           move      zero                 to   w-det-int-doc-num      .
       det-ger-cre-050.
      *              *-------------------------------------------------*
      *              * Test preliminare su codici in input             *
      *              *-------------------------------------------------*
           if        d-ird-acq-tip-dsr    =    zero or
                     d-ird-acq-prt-dsr    =    zero
                     go to det-ger-cre-900.
       det-ger-cre-060.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           perform   det-ger-doc-nor-000  thru det-ger-doc-nor-999    .
       det-ger-cre-200.
      *              *-------------------------------------------------*
      *              * I ciclo di scansione : generico                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * determinati per il tipo documento richiesto *
      *                  *---------------------------------------------*
           move      d-ird-acq-tip-dsr    to   w-det-int-doc-tdo      .
           move      d-ird-acq-prt-dsr    to   w-det-int-doc-pdo      .
           move      "1"                  to   w-det-int-doc-tek      .
           perform   det-ger-cre-ida-000  thru det-ger-cre-ida-999    .
      *                  *---------------------------------------------*
      *                  * Test sul numero di elementi trovati         *
      *                  *---------------------------------------------*
           if        w-det-int-doc-num    not  > zero
                     move  zero           to   d-ird-acq-num-ele
                     go to det-ger-cre-900.
       det-ger-cre-250.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo documento sor-  *
      *              * gente                                           *
      *              *-------------------------------------------------*
           if        d-ird-acq-tip-dsr    =    11
                     go to det-ger-cre-300
           else if   d-ird-acq-tip-dsr    =    21
                     go to det-ger-cre-400
           else if   d-ird-acq-tip-dsr    =    31
                     go to det-ger-cre-500.
       det-ger-cre-300.
      *              *-------------------------------------------------*
      *              * II ciclo di scansione : spedizioni              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * 'spedizione' determinati nella scansione    *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-cre-320.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-cre-400.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-cre-400.
           if        w-det-int-doc-prt
                    (w-det-int-doc-ctr)   =    zero
                     go to det-ger-cre-400.
           if        w-det-int-doc-tip
                    (w-det-int-doc-ctr)   not  = 11
                     go to det-ger-cre-320.
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           move      11                   to   w-det-int-doc-tdo      .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   w-det-int-doc-pdo      .
           move      "2"                  to   w-det-int-doc-tek      .
           perform   det-ger-cre-ida-000  thru det-ger-cre-ida-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo a 'bolla' successiva                *
      *                  *---------------------------------------------*
           go to     det-ger-cre-320.
       det-ger-cre-400.
      *              *-------------------------------------------------*
      *              * III ciclo di scansione : bolle                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * 'bolla' determinati nella scansione         *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-cre-420.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-cre-500.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-cre-500.
           if        w-det-int-doc-prt
                    (w-det-int-doc-ctr)   =    zero
                     go to det-ger-cre-500.
           if        w-det-int-doc-tip
                    (w-det-int-doc-ctr)   not  = 21
                     go to det-ger-cre-420.
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           move      21                   to   w-det-int-doc-tdo      .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   w-det-int-doc-pdo      .
           move      "3"                  to   w-det-int-doc-tek      .
           perform   det-ger-cre-ida-000  thru det-ger-cre-ida-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo a 'ods' successiva                  *
      *                  *---------------------------------------------*
           go to     det-ger-cre-420.
       det-ger-cre-500.
      *              *-------------------------------------------------*
      *              * IV ciclo di scansione : Fatture                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione di tutti gli elementi    *
      *                  * 'fattura' determinati nella scansione       *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-cre-520.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-cre-600.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-cre-600.
           if        w-det-int-doc-prt
                    (w-det-int-doc-ctr)   =    zero
                     go to det-ger-cre-600.
           if        w-det-int-doc-tip
                    (w-det-int-doc-ctr)   not  = 31
                     go to det-ger-cre-520.
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           move      31                   to   w-det-int-doc-tdo      .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   w-det-int-doc-pdo      .
           move      "3"                  to   w-det-int-doc-tek      .
           perform   det-ger-cre-ida-000  thru det-ger-cre-ida-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo a 'orc' successiva                  *
      *                  *---------------------------------------------*
           go to     det-ger-cre-520.
       det-ger-cre-600.
      *              *=================================================*
      *              * Ordinamento tabella                             *
      *              *-------------------------------------------------*
           perform   det-ger-doc-srt-000  thru det-ger-doc-srt-999    .
       det-ger-cre-800.
      *              *=================================================*
      *              * Castelletto determinato in area di output       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione castelletto                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-cre-820.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-num
                     go to det-ger-cre-900.
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-cre-900.
      *                  *---------------------------------------------*
      *                  * Tipo documento sorgente                     *
      *                  *---------------------------------------------*
           move      w-det-int-doc-tip
                    (w-det-int-doc-ctr)   to   d-ird-acq-tip-dtr
                                              (w-det-int-doc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in funzione del tipo di     *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           move      w-det-int-doc-tip
                    (w-det-int-doc-ctr)   to   w-det-int-doc-let      .
           perform   det-ger-let-doc-000  thru det-ger-let-doc-999    .
      *                  *---------------------------------------------*
      *                  * Aggiornamento contatore elementi            *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           add       1                    to   d-ird-acq-num-ele      .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-cre-820.
       det-ger-cre-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-cre-999.
       det-ger-cre-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento: gerarchia       *
      *    *                                           crescente       *
      *    *                                                           *
      *    * Subroutine per scansione generica [ida]                   *
      *    *-----------------------------------------------------------*
       det-ger-cre-ida-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per rottura              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-int-doc-rtp      .
           move      zero                 to   w-det-int-doc-rpr      .
       det-ger-cre-ida-100.
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "TRGSRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-int-doc-tdo    to   rf-ida-tip-dtr         .
           move      w-det-int-doc-pdo    to   rf-ida-prt-dtr         .
           move      zero                 to   rf-ida-prg-dtr         .
           move      zero                 to   rf-ida-tip-dsr         .
           move      zero                 to   rf-ida-prt-dsr         .
           move      zero                 to   rf-ida-prg-dsr         .
           move      "pgm/ffo/fls/ioc/obj/iofida"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ida                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : ad uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-cre-ida-900.
       det-ger-cre-ida-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [ida]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofida"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ida                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-cre-ida-900.
       det-ger-cre-ida-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : ad uscita                 *
      *              *-------------------------------------------------*
           if        rf-ida-tip-dtr       not  = w-det-int-doc-tdo or
                     rf-ida-prt-dtr       not  = w-det-int-doc-pdo
                     go to det-ger-cre-ida-900.
       det-ger-cre-ida-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca elemento castelletto protocolli     *
      *                  *---------------------------------------------*
           set       w-det-int-doc-inx    to   1                      .
           search    w-det-int-doc-cst
                     when    w-det-int-doc-tip
                            (w-det-int-doc-inx)
                                          =    rf-ida-tip-dsr and
                             w-det-int-doc-prt
                            (w-det-int-doc-inx)
                                          =    rf-ida-prt-dsr
                     go to   det-ger-cre-ida-200.
       det-ger-cre-ida-500.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento contatore documenti           *
      *                  *---------------------------------------------*
           add       1                    to   w-det-int-doc-num      .
      *                  *---------------------------------------------*
      *                  * Chiavi di ordinamento                       *
      *                  *---------------------------------------------*
           if        w-det-int-doc-tek    =    "1"
                     move  rf-ida-prt-dsr to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "2"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
                     move  rf-ida-prt-dsr to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "3"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
                     move  rf-ida-prt-dsr to   w-det-int-doc-k03
                                              (w-det-int-doc-num)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
           move      rf-ida-tip-dsr       to   w-det-int-doc-tip
                                              (w-det-int-doc-num)     .
           move      rf-ida-prt-dsr       to   w-det-int-doc-prt
                                              (w-det-int-doc-num)     .
       det-ger-cre-ida-700.
      *              *-------------------------------------------------*
      *              * Trattamento elemento di ultimo livello per as-  *
      *              * sociare il protocollo in chiave mancante        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ultimo livello (fattura)            *
      *                  *---------------------------------------------*
           if        w-det-int-doc-tek    not  = "3"
                     go to det-ger-cre-ida-800.
      *                  *---------------------------------------------*
      *                  * Ricerca elemento castelletto protocolli     *
      *                  *---------------------------------------------*
           set       w-det-int-doc-inx    to   1                      .
           search    w-det-int-doc-cst
                     when    w-det-int-doc-tip
                            (w-det-int-doc-inx)
                                          =    w-det-int-doc-tdo and
                             w-det-int-doc-prt
                            (w-det-int-doc-inx)
                                          =    w-det-int-doc-pdo
                     go to   det-ger-cre-ida-720.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-cre-ida-800.
       det-ger-cre-ida-720.
      *                  *---------------------------------------------*
      *                  * Se elemento non trovato: completamento      *
      *                  * chiave di ordinamento                       *
      *                  *---------------------------------------------*
           move      w-det-int-doc-k01
                    (w-det-int-doc-inx)   to   w-det-int-doc-k01
                                              (w-det-int-doc-num)     .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-cre-ida-800.
       det-ger-cre-ida-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ger-cre-ida-200.
       det-ger-cre-ida-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-cre-ida-999.
       det-ger-cre-ida-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune di normalizzazione preliminare          *
      *    *-----------------------------------------------------------*
       det-ger-doc-nor-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Buffer elementi                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-ctr      .
       det-ger-doc-nor-200.
           add       1                    to   w-det-int-doc-ctr      .
           if        w-det-int-doc-ctr    >    w-det-int-doc-max
                     go to det-ger-doc-nor-900.
      *                  *---------------------------------------------*
      *                  * Buffer elementi in output                   *
      *                  *---------------------------------------------*
           move      zero                 to   d-ird-acq-tip-dtr
                                              (w-det-int-doc-ctr)     .
           move      spaces               to   d-ird-acq-tmo-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-acq-prt-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-acq-prg-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-acq-num-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-acq-dat-dtr
                                              (w-det-int-doc-ctr)     .
           move      spaces               to   d-ird-acq-sts-dtr
                                              (w-det-int-doc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Buffer elementi in work                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-int-doc-k01
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   w-det-int-doc-k02
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   w-det-int-doc-k03
                                              (w-det-int-doc-ctr)     .
      *
           move      zero                 to   w-det-int-doc-tip
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   w-det-int-doc-prt
                                              (w-det-int-doc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-ger-doc-nor-200.
       det-ger-doc-nor-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-doc-nor-999.
       det-ger-doc-nor-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *                                               *
      *    *                                                           *
      *    * Subroutine comune di ordinamento righe bufferizzate       *
      *    *-----------------------------------------------------------*
       det-ger-doc-srt-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-det-int-doc-num    <    2
                     go to det-ger-doc-srt-999.
       det-ger-doc-srt-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-int-doc-c01      .
       det-ger-doc-srt-100.
           add       1                    to   w-det-int-doc-c01      .
           if        w-det-int-doc-c01    =    w-det-int-doc-num
                     go to det-ger-doc-srt-999.
           move      w-det-int-doc-c01    to   w-det-int-doc-c02
                                               w-det-int-doc-c03      .
           move      w-det-int-doc-key
                    (w-det-int-doc-c01)   to   w-det-int-doc-svk      .
       det-ger-doc-srt-200.
           add       1                    to   w-det-int-doc-c02      .
           if        w-det-int-doc-c02    >    w-det-int-doc-num
                     go to det-ger-doc-srt-300.
           if        w-det-int-doc-key
                    (w-det-int-doc-c02)   >    w-det-int-doc-svk
                     go to det-ger-doc-srt-200.
           move      w-det-int-doc-c02    to   w-det-int-doc-c03      .
           move      w-det-int-doc-key
                    (w-det-int-doc-c02)   to   w-det-int-doc-svk      .
           go to     det-ger-doc-srt-200.
       det-ger-doc-srt-300.
           move      w-det-int-doc-c01    to   w-det-int-doc-c04      .          
           if        w-det-int-doc-svk    >    w-det-int-doc-key
                                              (w-det-int-doc-c04)
                     go to det-ger-doc-srt-100.
           move      w-det-int-doc-cst
                    (w-det-int-doc-c03)   to   w-det-int-doc-cst
                                              (w-det-int-doc-max)     .
           move      w-det-int-doc-cst
                    (w-det-int-doc-c04)   to   w-det-int-doc-cst
                                              (w-det-int-doc-c03)     .
           move      w-det-int-doc-cst
                    (w-det-int-doc-max)   to   w-det-int-doc-cst
                                              (w-det-int-doc-c04)     .
           go to     det-ger-doc-srt-100.
       det-ger-doc-srt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune per lettura dati documenti              *
      *    *-----------------------------------------------------------*
       det-ger-let-doc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo documento       *
      *              *-------------------------------------------------*
           if        w-det-int-doc-let    =    01
                     perform  det-ger-let-orc-000
                                          thru det-ger-let-orc-999
           else if   w-det-int-doc-let    =    21
                     perform  det-ger-let-bfo-000
                                          thru det-ger-let-bfo-999
           else if   w-det-int-doc-let    =    31
                     perform  det-ger-let-ffo-000
                                          thru det-ger-let-ffo-999
           else      go to det-ger-let-doc-900.
       det-ger-let-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-doc-999.
       det-ger-let-doc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune per bufferizzazione dati ordine         *
      *    *-----------------------------------------------------------*
       det-ger-let-orc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [oft]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Lettura [oft]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-oft-tmo-orf       to   d-ird-acq-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-acq-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-acq-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      rf-oft-num-doc       to   d-ird-acq-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-oft-dat-doc       to   d-ird-acq-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-sts-orf-tip-ope      .
           perform   det-sts-orf-cll-000  thru det-sts-orf-cll-999    .
______*    move      d-sts-orf-sts-lit    to   d-ird-acq-sts-dtr
______*                                       (w-det-int-doc-ctr)     .
           move      spaces               to   d-ird-acq-sts-dtr
                                              (w-det-int-doc-ctr)     .
       det-ger-let-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-orc-999.
       det-ger-let-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune per bufferizzazione dati bolla          *
      *    *-----------------------------------------------------------*
       det-ger-let-bfo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [bft]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Lettura [bft]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-bft-cod-tmb       to   d-ird-acq-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-acq-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-acq-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      rf-bft-num-doc       to   d-ird-acq-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-bft-dat-doc       to   d-ird-acq-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           move      spaces               to   d-ird-acq-sts-dtr
                                              (w-det-int-doc-ctr)     .
       det-ger-let-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-bfo-999.
       det-ger-let-bfo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune per bufferizzazione dati fattura        *
      *    *-----------------------------------------------------------*
       det-ger-let-ffo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [fft]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *              *-------------------------------------------------*
      *              * Lettura [fft]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-fft-num-prt         .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-fft-cod-tmf       to   d-ird-acq-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-acq-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-acq-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      rf-fft-num-doc       to   d-ird-acq-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-fft-dat-doc       to   d-ird-acq-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           move      spaces               to   d-ird-acq-sts-dtr
                                              (w-det-int-doc-ctr)     .
       det-ger-let-ffo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-ffo-999.
       det-ger-let-ffo-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine fornitore     *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dstsorf0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


