       Identification Division.
       Program-Id.                                 dirdven0           .
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
      *        Input  : d-ird-ven-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-ird-ven-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-ird-ven-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-ird-ven-exi-sts = spaces: Si                 *
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
      *        Input  : d-ird-ven-tip-ope = "D<"                       *
      *                                                                *
      *                 d-ird-ven-tip-dsr = Tipo documento             *
      *                                                                *
      *                 d-ird-ven-prt-dsr = Protocollo documento       *
      *                                                                *
      *                 d-ird-ven-prg-dsr = Progressivo documento      *
      *                                    (facoltativo)               *
      *                                                                *
      *                                                                *
      *        Output : d-ird-ven-buf-ele = Buffer documenti correlati *
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
      *        Input  : d-ird-ven-tip-ope = "D>"                       *
      *                                                                *
      *                 d-ird-ven-tip-dsr = Tipo documento             *
      *                                                                *
      *                 d-ird-ven-prt-dsr = Protocollo documento       *
      *                                                                *
      *                 d-ird-ven-prg-dsr = Progressivo documento      *
      *                                    (facoltativo)               *
      *                                                                *
      *                                                                *
      *        Output : d-ird-ven-buf-ele = Buffer documenti correlati *
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
      *        * [idv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfidv"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .
      *        *-------------------------------------------------------*
      *        * [ddx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfddx"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .

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
      *        *-------------------------------------------------------*
      *        * Determinazione se documento gia' in distinta          *
      *        *-------------------------------------------------------*
           05  w-det-dst-num-prt          pic  9(09)                  .
           05  w-det-dst-num-dst          pic  9(09)                  .
           05  w-det-dst-sts-dst          pic  x(40)                  .
           05  w-det-dst-ide-sdi          pic  x(20)                  .
           05  w-det-dst-ctr-ele          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfi.
               10  w-let-arc-zfi-flg      pic  x(01)                  .
               10  w-let-arc-zfi-cod      pic  x(05)                  .
               10  w-let-arc-zfi-des      pic  x(30)                  .
               10  w-let-arc-zfi-dst      pic  x(30)                  .
               10  w-let-arc-zfi-vld      pic  9(02)                  .
               10  w-let-arc-zfi-dpz      pic  9(02)                  .
               10  w-let-arc-zfi-tdo      pic  9(02)                  .
               10  w-let-arc-zfi-ord      pic  9(02)                  .
               10  w-let-arc-zfi-prd      pic  9(02)                  .
               10  w-let-arc-zfi-ngi      pic  9(02)                  .
               10  w-let-arc-zfi-sgl      pic  x(03)                  .
               10  w-let-arc-zfi-cau      pic  9(03)                  .
               10  w-let-arc-zfi-siv      pic  9(07)                  .
               10  w-let-arc-zfi-sve      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per ripristino Start                            *
      *    *-----------------------------------------------------------*
       01  w-rip-str.
      *        *-------------------------------------------------------*
      *        * Work-area per ripristino Start su [idv]               *
      *        *-------------------------------------------------------*
           05  w-rip-str-fit.
      *            *---------------------------------------------------*
      *            * Flag di End-of-file                               *
      *            *---------------------------------------------------*
               10  w-rip-str-fit-eof      pic  x(01)                  .
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
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dstsods0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dtl"                   .

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
           copy      "pgm/fat/prg/cpy/dirdven0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-ird-ven              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-ird-ven-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione gerarchia decrescente        *
      *                  * (da Fattura a Ordine)                       *
      *                  *---------------------------------------------*
           if        d-ird-ven-tip-ope    =    "D<"
                     perform det-ger-dec-000
                                          thru det-ger-dec-999
      *                  *---------------------------------------------*
      *                  * Determinazione gerarchia crescente          *
      *                  * (da Ordine a Fattura)                       *
      *                  *---------------------------------------------*
           else if   d-ird-ven-tip-ope    =    "D>"
                     perform det-ger-cre-000
                                          thru det-ger-cre-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-ird-ven-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-ird-ven-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-ird-ven-tip-ope    =    "C?"
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
      *              * [idv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofidv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-idv                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * [ddx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofddx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddx                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [ost]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine di  *
      *              * spedizione                                      *
      *              *-------------------------------------------------*
           perform   det-sts-ods-opn-000  thru det-sts-ods-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine     *
      *              *-------------------------------------------------*
           perform   det-sts-orc-opn-000  thru det-sts-orc-opn-999    .
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
      *              * [idv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofidv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-idv                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * [ddx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofddx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddx                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [ost]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine di *
      *              * spedizione                                      *
      *              *-------------------------------------------------*
           perform   det-sts-ods-cls-000  thru det-sts-ods-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              *-------------------------------------------------*
           perform   det-sts-orc-cls-000  thru det-sts-orc-cls-999    .
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
                     move  spaces         to   d-ird-ven-exi-sts
           else      move  "#"            to   d-ird-ven-exi-sts      .
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
           move      zero                 to   d-ird-ven-num-ele      .
           move      zero                 to   w-det-int-doc-num      .
       det-ger-dec-050.
      *              *-------------------------------------------------*
      *              * Test preliminare su codici in input             *
      *              *-------------------------------------------------*
           if        d-ird-ven-tip-dsr    =    zero or
                     d-ird-ven-prt-dsr    =    zero
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
           move      d-ird-ven-tip-dsr    to   w-det-int-doc-tdo      .
           move      d-ird-ven-prt-dsr    to   w-det-int-doc-pdo      .
           move      "1"                  to   w-det-int-doc-tek      .
           perform   det-ger-dec-idv-000  thru det-ger-dec-idv-999    .
      *                  *---------------------------------------------*
      *                  * Test sul numero di elementi trovati         *
      *                  *---------------------------------------------*
           if        w-det-int-doc-num    not  > zero
                     move  zero           to   d-ird-ven-num-ele
                     go to det-ger-dec-900.
       det-ger-dec-250.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo documento sor-  *
      *              * gente                                           *
      *              *-------------------------------------------------*
           if        d-ird-ven-tip-dsr    =    31
                     go to det-ger-dec-300
           else if   d-ird-ven-tip-dsr    =    21
                     go to det-ger-dec-400
           else if   d-ird-ven-tip-dsr    =    11
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
           perform   det-ger-dec-idv-000  thru det-ger-dec-idv-999    .
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
           perform   det-ger-dec-idv-000  thru det-ger-dec-idv-999    .
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
           perform   det-ger-dec-idv-000  thru det-ger-dec-idv-999    .
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
                    (w-det-int-doc-ctr)   to   d-ird-ven-tip-dtr
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
           add       1                    to   d-ird-ven-num-ele      .
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
      *    * Subroutine per scansione generica [idv]                   *
      *    *-----------------------------------------------------------*
       det-ger-dec-idv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per rottura              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-int-doc-rtp      .
           move      zero                 to   w-det-int-doc-rpr      .
       det-ger-dec-idv-100.
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SRGTRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-int-doc-tdo    to   rf-idv-tip-dsr         .
           move      w-det-int-doc-pdo    to   rf-idv-prt-dsr         .
           move      zero                 to   rf-idv-prg-dsr         .
           move      zero                 to   rf-idv-tip-dtr         .
           move      zero                 to   rf-idv-prt-dtr         .
           move      zero                 to   rf-idv-prg-dtr         .
           move      "pgm/fat/fls/ioc/obj/iofidv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-idv                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : ad uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-dec-idv-900.
       det-ger-dec-idv-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [idv]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofidv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-idv                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-dec-idv-900.
       det-ger-dec-idv-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : ad uscita                 *
      *              *-------------------------------------------------*
           if        rf-idv-tip-dsr       not  = w-det-int-doc-tdo or
                     rf-idv-prt-dsr       not  = w-det-int-doc-pdo
                     go to det-ger-dec-idv-900.
       det-ger-dec-idv-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-ger-dec-idv-500.
      *              *-------------------------------------------------*
      *              * Test di rottura                                 *
      *              *-------------------------------------------------*
           if        rf-idv-tip-dtr       =    w-det-int-doc-rtp and
                     rf-idv-prt-dtr       =    w-det-int-doc-rpr
                     go to det-ger-dec-idv-200.
       det-ger-dec-idv-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento rottura                           *
      *              *-------------------------------------------------*
           move      rf-idv-tip-dtr       to   w-det-int-doc-rtp      .
           move      rf-idv-prt-dtr       to   w-det-int-doc-rpr      .
       det-ger-dec-idv-650.
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
                     move  rf-idv-prt-dtr to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "2"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
                     move  rf-idv-prt-dtr to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "3"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
                     move  rf-idv-prt-dtr to   w-det-int-doc-k03
                                              (w-det-int-doc-num)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
           move      rf-idv-tip-dtr       to   w-det-int-doc-tip
                                              (w-det-int-doc-num)     .
           move      rf-idv-prt-dtr       to   w-det-int-doc-prt
                                              (w-det-int-doc-num)     .
       det-ger-dec-idv-700.
      *              *-------------------------------------------------*
      *              * Trattamento elemento di ultimo livello per as-  *
      *              * sociare il protocollo in chiave mancante        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ultimo livello (ordine)             *
      *                  *---------------------------------------------*
           if        w-det-int-doc-tek    not  = "3"
                     go to det-ger-dec-idv-800.
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
                     go to   det-ger-dec-idv-720.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-dec-idv-800.
       det-ger-dec-idv-720.
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
           go to     det-ger-dec-idv-800.
       det-ger-dec-idv-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ger-dec-idv-200.
       det-ger-dec-idv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-dec-idv-999.
       det-ger-dec-idv-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento: gerarchia       *
      *    *                                           crescente       *
      *    *-----------------------------------------------------------*
       det-ger-cre-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-ven-num-ele      .
           move      zero                 to   w-det-int-doc-num      .
       det-ger-cre-050.
      *              *-------------------------------------------------*
      *              * Test preliminare su codici in input             *
      *              *-------------------------------------------------*
           if        d-ird-ven-tip-dsr    =    zero or
                     d-ird-ven-prt-dsr    =    zero
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
           move      d-ird-ven-tip-dsr    to   w-det-int-doc-tdo      .
           move      d-ird-ven-prt-dsr    to   w-det-int-doc-pdo      .
           move      "1"                  to   w-det-int-doc-tek      .
           perform   det-ger-cre-idv-000  thru det-ger-cre-idv-999    .
      *                  *---------------------------------------------*
      *                  * Test sul numero di elementi trovati         *
      *                  *---------------------------------------------*
           if        w-det-int-doc-num    not  > zero
                     move  zero           to   d-ird-ven-num-ele
                     go to det-ger-cre-900.
       det-ger-cre-250.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo documento sor-  *
      *              * gente                                           *
      *              *-------------------------------------------------*
           if        d-ird-ven-tip-dsr    =    11
                     go to det-ger-cre-300
           else if   d-ird-ven-tip-dsr    =    21
                     go to det-ger-cre-400
           else if   d-ird-ven-tip-dsr    =    31
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
           perform   det-ger-cre-idv-000  thru det-ger-cre-idv-999    .
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
           perform   det-ger-cre-idv-000  thru det-ger-cre-idv-999    .
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
           perform   det-ger-cre-idv-000  thru det-ger-cre-idv-999    .
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
                    (w-det-int-doc-ctr)   to   d-ird-ven-tip-dtr
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
           add       1                    to   d-ird-ven-num-ele      .
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
      *    * Subroutine per scansione generica [idv]                   *
      *    *-----------------------------------------------------------*
       det-ger-cre-idv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per rottura              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-int-doc-rtp      .
           move      zero                 to   w-det-int-doc-rpr      .
       det-ger-cre-idv-100.
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "TRGSRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-int-doc-tdo    to   rf-idv-tip-dtr         .
           move      w-det-int-doc-pdo    to   rf-idv-prt-dtr         .
           move      zero                 to   rf-idv-prg-dtr         .
           move      zero                 to   rf-idv-tip-dsr         .
           move      zero                 to   rf-idv-prt-dsr         .
           move      zero                 to   rf-idv-prg-dsr         .
           move      "pgm/fat/fls/ioc/obj/iofidv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-idv                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : ad uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-cre-idv-900.
       det-ger-cre-idv-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [idv]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofidv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-idv                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-cre-idv-900.
       det-ger-cre-idv-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : ad uscita                 *
      *              *-------------------------------------------------*
           if        rf-idv-tip-dtr       not  = w-det-int-doc-tdo or
                     rf-idv-prt-dtr       not  = w-det-int-doc-pdo
                     go to det-ger-cre-idv-900.
       det-ger-cre-idv-400.
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
                                          =    rf-idv-tip-dsr and
                             w-det-int-doc-prt
                            (w-det-int-doc-inx)
                                          =    rf-idv-prt-dsr
                     go to   det-ger-cre-idv-200.
       det-ger-cre-idv-500.
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
                     move  rf-idv-prt-dsr to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "2"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k01
                                              (w-det-int-doc-num)
                     move  rf-idv-prt-dsr to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
           else if   w-det-int-doc-tek    =    "3"
                     move  w-det-int-doc-pdo
                                          to   w-det-int-doc-k02
                                              (w-det-int-doc-num)
                     move  rf-idv-prt-dsr to   w-det-int-doc-k03
                                              (w-det-int-doc-num)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
           move      rf-idv-tip-dsr       to   w-det-int-doc-tip
                                              (w-det-int-doc-num)     .
           move      rf-idv-prt-dsr       to   w-det-int-doc-prt
                                              (w-det-int-doc-num)     .
       det-ger-cre-idv-700.
      *              *-------------------------------------------------*
      *              * Trattamento elemento di ultimo livello per as-  *
      *              * sociare il protocollo in chiave mancante        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ultimo livello (fattura)            *
      *                  *---------------------------------------------*
           if        w-det-int-doc-tek    not  = "3"
                     go to det-ger-cre-idv-800.
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
                     go to   det-ger-cre-idv-720.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-ger-cre-idv-800.
       det-ger-cre-idv-720.
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
           go to     det-ger-cre-idv-800.
       det-ger-cre-idv-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ger-cre-idv-200.
       det-ger-cre-idv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-cre-idv-999.
       det-ger-cre-idv-999.
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
           move      zero                 to   d-ird-ven-tip-dtr
                                              (w-det-int-doc-ctr)     .
           move      spaces               to   d-ird-ven-tmo-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-ven-prt-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-ven-prg-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-ven-num-dtr
                                              (w-det-int-doc-ctr)     .
           move      zero                 to   d-ird-ven-dat-dtr
                                              (w-det-int-doc-ctr)     .
           move      spaces               to   d-ird-ven-sts-dtr
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
           else if   w-det-int-doc-let    =    11
                     perform  det-ger-let-ods-000
                                          thru det-ger-let-ods-999
           else if   w-det-int-doc-let    =    21
                     perform  det-ger-let-bol-000
                                          thru det-ger-let-bol-999
           else if   w-det-int-doc-let    =    31
                     perform  det-ger-let-fat-000
                                          thru det-ger-let-fat-999
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
      *              * Normalizzazione [oct]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Lettura [oct]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-oct-tmo-orc       to   d-ird-ven-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-ven-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-ven-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      rf-oct-num-doc       to   d-ird-ven-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-oct-dat-doc       to   d-ird-ven-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-sts-orc-tip-ope      .
           perform   det-sts-orc-cll-000  thru det-sts-orc-cll-999    .
           move      d-sts-orc-sts-lit    to   d-ird-ven-sts-dtr
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
      *    * Subroutine comune per bufferizzazione dati spedizione     *
      *    *-----------------------------------------------------------*
       det-ger-let-ods-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [ost]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * Lettura [ost]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-ost-num-prt         .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-ost-cod-tms       to   d-ird-ven-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-ven-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-ven-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-ven-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-ost-dat-doc       to   d-ird-ven-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-sts-ods-tip-ope      .
           perform   det-sts-ods-cll-000  thru det-sts-ods-cll-999    .
      *
           if        d-sts-ods-sts-ord    =    01
                     move  "Chiuso                      "
                                          to   d-ird-ven-sts-dtr
                                              (w-det-int-doc-ctr)
           else if   d-sts-ods-sts-ord    =    02
                     move  "Evaso parzialmente          "
                                          to   d-ird-ven-sts-dtr
                                              (w-det-int-doc-ctr)
           else if   d-sts-ods-sts-ord    =    03
                     move  "Evaso                       "
                                          to   d-ird-ven-sts-dtr
                                              (w-det-int-doc-ctr)
           else if   d-sts-ods-sts-ord    =    04
                     move  "Inevaso                     "
                                          to   d-ird-ven-sts-dtr
                                              (w-det-int-doc-ctr)     .
       det-ger-let-ods-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-ods-999.
       det-ger-let-ods-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune per bufferizzazione dati bolla          *
      *    *-----------------------------------------------------------*
       det-ger-let-bol-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [bit]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Lettura [bit]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-bit-cod-tmb       to   d-ird-ven-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-ven-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-ven-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      rf-bit-num-doc       to   d-ird-ven-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-bit-dat-doc       to   d-ird-ven-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           perform   det-ger-let-bst-000  thru det-ger-let-bst-999    .
           move      w-det-ann-doc-des    to   d-ird-ven-sts-dtr
                                              (w-det-int-doc-ctr)     .
       det-ger-let-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-bol-999.
       det-ger-let-bol-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine per la determinazione dello status bolle       *
      *    *-----------------------------------------------------------*
       det-ger-let-bst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-ann-doc-des      .
       det-ger-let-bst-050.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di interesse per  *
      *              * la fatturazione                                 *
      *              *-------------------------------------------------*
           go to     det-ger-let-bst-100
                     det-ger-let-bst-200
                     det-ger-let-bst-300
                     depending            on    rf-bit-int-ftr        .
           go to     det-ger-let-bst-900.
       det-ger-let-bst-100.
      *              *-------------------------------------------------*
      *              * Se documento non interessa la fatturazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "Non fatturabile"    to   w-det-ann-doc-des      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-ger-let-bst-900.
       det-ger-let-bst-200.
      *              *-------------------------------------------------*
      *              * Se documento sottoponibile a fatturazione       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che sia stato fattura- *
      *                  * to o no                                     *
      *                  *---------------------------------------------*
           if        rf-bit-fat-dat       =    zero and
                     rf-bit-fat-num       =    zero
                     go to det-ger-let-bst-220.
      *                  *---------------------------------------------*
      *                  * Se e' stato fatturato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura del tipo di movimento per la    *
      *                      * fatturazione                            *
      *                      *-----------------------------------------*
           move      rf-bit-tmo-ftr       to   w-let-arc-zfi-cod      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
      *                      *-----------------------------------------*
      *                      * Editing numero fattura                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-bit-fat-num       to   w-wrk-num-doc          .
           move      w-wrk-ndo-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-ann-doc-ned      .
      *                      *-----------------------------------------*
      *                      * Editing data fattura                    *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-bit-fat-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-ann-doc-ded      .
      *                      *-----------------------------------------*
      *                      * Composizione stringa                    *
      *                      *-----------------------------------------*
           move      27                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           if        w-let-arc-zfi-tdo    =    01
                     move  "Ft."          to   w-all-str-cat (1)
           else if   w-let-arc-zfi-tdo    =    02
                     move  "Nd."          to   w-all-str-cat (1)
           else if   w-let-arc-zfi-tdo    =    03
                     move  "Nc."          to   w-all-str-cat (1)
           else      move  "Ft."          to   w-all-str-cat (1)      .
           move      w-det-ann-doc-ned    to   w-all-str-cat (2)      .
           move      "del"                to   w-all-str-cat (3)      .
           move      w-det-ann-doc-ded    to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-det-ann-doc-des      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-ger-let-bst-900.
       det-ger-let-bst-220.
      *                  *---------------------------------------------*
      *                  * Se non e' ancora stato sottoposto a fattura *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "Da fatturare"       to   w-det-ann-doc-des      .
      *                      *-----------------------------------------*
      *                      * Eventuale segnale di momentanea esclu-  *
      *                      * sione del documento dalla fatturazione  *
      *                      * differita automatica                    *
      *                      *-----------------------------------------*
           if        rf-bit-flg-nbx (1)   =    spaces
                     go to det-ger-let-bst-900.
      *                      *-----------------------------------------*
      *                      * Concatenamento                          *
      *                      *-----------------------------------------*
           move      27                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-det-ann-doc-des    to   w-all-str-cat (1)      .
           move      "(in sospeso)"       to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-det-ann-doc-des      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-ger-let-bst-900.
       det-ger-let-bst-300.
      *              *-------------------------------------------------*
      *              * Se documento che e' anche fattura               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-ger-let-bst-900.
       det-ger-let-bst-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-bst-999.
       det-ger-let-bst-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine comune per bufferizzazione dati fattura        *
      *    *-----------------------------------------------------------*
       det-ger-let-fat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [fit]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Lettura [fit]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           move      rf-fit-cod-tmo       to   d-ird-ven-tmo-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           move      w-det-int-doc-prt
                    (w-det-int-doc-ctr)   to   d-ird-ven-prt-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-ird-ven-prg-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      rf-fit-num-doc       to   d-ird-ven-num-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           move      rf-fit-dat-doc       to   d-ird-ven-dat-dtr
                                              (w-det-int-doc-ctr)     .
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           perform   det-ger-let-fst-000  thru det-ger-let-fst-999    .
           move      w-det-ann-doc-des    to   d-ird-ven-sts-dtr
                                              (w-det-int-doc-ctr)     .
       det-ger-let-fat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-fat-999.
       det-ger-let-fat-999.
           exit.

      *    *===========================================================*
      *    * Determinazione inter-relazioni documento                  *
      *    *                                                           *
      *    * Subroutine per la determinazione dello status fattura     *
      *    *-----------------------------------------------------------*
       det-ger-let-fst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-ann-doc-des      .
       det-ger-let-fst-100.
      *              *-------------------------------------------------*
      *              * Start su [ddx]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DOCDST    "         to   f-key                  .
           move      rf-fit-num-prt       to   rf-ddx-prt-doc         .
           move      zero                 to   rf-ddx-num-dst         .
           move      zero                 to   rf-ddx-num-prg         .
           move      "pgm/fat/fls/ioc/obj/iofddx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddx                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-let-fst-900.
       det-ger-let-fst-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [ddx]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofddx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddx                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ger-let-fst-900.
       det-ger-let-fst-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ddx-prt-doc       not  = rf-fit-num-prt
                     go to det-ger-let-fst-900.
       det-ger-let-fst-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se documento scartato                  *
      *                  *---------------------------------------------*
           if        rf-ddx-flg-esi       not  = spaces
                     go to det-ger-let-fst-200.
      *                  *---------------------------------------------*
      *                  * Test se avvenuto invio                      *
      *                  *---------------------------------------------*
           if        rf-ddx-dat-inv       =    zero and
                     rf-ddx-flg-ain       not  = spaces
                     go to det-ger-let-fst-200.
       det-ger-let-fst-600.
      *              *-------------------------------------------------*
      *              * Editing distinta                                *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-ddx-num-dst       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Composizione stringa                            *
      *              *-------------------------------------------------*
           move      27                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Distinta SDI"       to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-det-ann-doc-des      .
       det-ger-let-fst-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ger-let-fst-999.
       det-ger-let-fst-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zfi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zfi-cod    =    spaces
                     go to let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      w-let-arc-zfi-cod    to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfi-400.
       let-arc-zfi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfi-des-tmo       to   w-let-arc-zfi-des      .
           move      rf-zfi-des-stp       to   w-let-arc-zfi-dst      .
           move      rf-zfi-vld-dpz       to   w-let-arc-zfi-vld      .
           move      rf-zfi-cod-dpz       to   w-let-arc-zfi-dpz      .
           move      rf-zfi-tip-doc       to   w-let-arc-zfi-tdo      .
           move      rf-zfi-org-doc       to   w-let-arc-zfi-ord      .
           move      rf-zfi-prv-doc       to   w-let-arc-zfi-prd      .
           move      rf-zfi-num-giv       to   w-let-arc-zfi-ngi      .
           move      rf-zfi-sgl-num       to   w-let-arc-zfi-sgl      .
           move      rf-zfi-cau-cge       to   w-let-arc-zfi-cau      .
           move      rf-zfi-ctp-ivv       to   w-let-arc-zfi-siv      .
           move      rf-zfi-ctp-ven       to   w-let-arc-zfi-sve      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfi-999.
       let-arc-zfi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfi-flg      .
           move      all   "."            to   w-let-arc-zfi-des      .
           go to     let-arc-zfi-520.
       let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-des      .
       let-arc-zfi-520.
           move      spaces               to   w-let-arc-zfi-dst      .
           move      zero                 to   w-let-arc-zfi-vld      .
           move      zero                 to   w-let-arc-zfi-dpz      .
           move      zero                 to   w-let-arc-zfi-tdo      .
           move      zero                 to   w-let-arc-zfi-ord      .
           move      zero                 to   w-let-arc-zfi-prd      .
           move      zero                 to   w-let-arc-zfi-ngi      .
           move      spaces               to   w-let-arc-zfi-sgl      .
           move      zero                 to   w-let-arc-zfi-cau      .
           move      zero                 to   w-let-arc-zfi-siv      .
           move      zero                 to   w-let-arc-zfi-sve      .
       let-arc-zfi-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine di spedizione *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dstsods0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine cliente       *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


