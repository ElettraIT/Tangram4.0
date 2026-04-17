       Identification Division.
       Program-Id.                                 mgescdp0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cdp                 *
      *                                Settore:                        *
      *                                   Fase:    gescdp              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/04/21    *
      *                       Ultima revisione:    NdK del 04/04/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la gestione delle commesse di produzione legate ad  *
      * un ordine cliente                                              *
      *                                                                *
      * Richiamato da 'porc3000'                                       *
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
      *        Input  : m-ges-cdp-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : m-ges-cdp-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : m-ges-cdp-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : m-ges-cdp-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "WC" - Scrittura commessa di produzione                        *
      *                                                                *
      *                                                                *
      *        Input  : m-ges-cdp-tip-ope = "WC"                       *
      *                                                                *
      *                 m-ges-cdp-cod-dpz = codice dipendenza          *
      *                                                                *
      *                 m-ges-cdp-dat-orc = data ordine cliente        *
      *                                                                *
      *                 m-ges-cdp-prt-orc = protocollo ordine cliente  *
      *                                                                *
      *                 m-ges-cdp-prg-orc = progressivo riga ordine    *
      *                                                                *
      *                 m-ges-cdp-cod-cli = codice cliente             *
      *                                                                *
      *                 m-ges-cdp-dpz-cli = codice dipendenza cliente  *
      *                                                                *
      *                 m-ges-cdp-num-pro = codice numerico prodotto   *
      *                                                                *
      *                 m-ges-cdp-dcn-prv = data consegna prevista     *
      *                                                                *
      *                 m-ges-cdp-qta-ord = quantita' ordinata         *
      *                                                                *
      *                                                                *
      *        Output : m-ges-cdp-exi-sts = spaces: generazione OK     *
      *                                     #     : errore             *
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
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cdp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfcdp"                          .
      *        *-------------------------------------------------------*
      *        * [cpx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfcpx"                          .
      *        *-------------------------------------------------------*
      *        * [cdpprt]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/num/rec/rncdpprt"                       .
      *        *-------------------------------------------------------*
      *        * [cdpndo]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/num/rec/rncdpndo"                       .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [lgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgt"                          .

      *    *===========================================================*
      *    * Work per subroutines di Att                               *
      *    *-----------------------------------------------------------*
       01  w-att.
      *        *-------------------------------------------------------*
      *        * Work per Att numero documento commesse di produzione  *
      *        *-------------------------------------------------------*
           05  w-att-cdp-ndo.
               10  w-att-cdp-ndo-flg      pic  x(01)                  .
               10  w-att-cdp-ndo-dat      pic  9(07)                  .
               10  w-att-cdp-ndo-saa      pic  9(03)                  .
               10  w-att-cdp-ndo-dpz      pic  9(02)                  .
               10  w-att-cdp-ndo-sgl      pic  x(03)                  .
               10  w-att-cdp-ndo-num      pic  9(11)                  .
               10  w-att-cdp-ndo-num-r    redefines
                   w-att-cdp-ndo-num.
                   15  filler             pic  9(03)                  .
                   15  filler             pic  9(02)                  .
                   15  w-att-cdp-ndo-prg  pic  9(06)                  .
               10  w-att-cdp-ndo-wnu      pic  9(11)                  .
               10  w-att-cdp-ndo-wnu-r    redefines
                   w-att-cdp-ndo-wnu.
                   15  w-att-cdp-ndo-wsa  pic  9(03)                  .
                   15  w-att-cdp-ndo-wdz  pic  9(02)                  .
                   15  w-att-cdp-ndo-wpr  pic  9(06)                  .
               10  w-att-cdp-ndo-new      pic  9(11)                  .
               10  w-att-cdp-ndo-nsv      pic  9(11)                  .
               10  w-att-cdp-ndo-dsv      pic  9(07)                  .
               10  w-att-cdp-ndo-wdf      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Work per Att numero protocollo commesse di produzione *
      *        *-------------------------------------------------------*
           05  w-att-cdp-prt.
               10  w-att-cdp-prt-dat      pic  9(07)                  .
               10  w-att-cdp-prt-saa      pic  9(03)                  .
               10  w-att-cdp-prt-dpz      pic  9(02)                  .
               10  w-att-cdp-prt-num      pic  9(11)                  .
               10  w-att-cdp-prt-wnu      pic  9(11)                  .
               10  w-att-cdp-prt-wnu-r    redefines
                   w-att-cdp-prt-wnu.
                   15  w-att-cdp-prt-wsa  pic  9(03)                  .
                   15  w-att-cdp-prt-wdz  pic  9(02)                  .
                   15  w-att-cdp-prt-wpr  pic  9(06)                  .

      *    *===========================================================*
      *    * Work per l'esecuzione del modulo                          *
      *    *-----------------------------------------------------------*
       01  w-aux-wcp.
      *        *-------------------------------------------------------*
      *        * Protocollo commessa                                   *
      *        *-------------------------------------------------------*
           05  w-aux-wcp-num-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Numero commessa                                       *
      *        *-------------------------------------------------------*
           05  w-aux-wcp-num-doc          pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per valori di default                           *
      *    *-----------------------------------------------------------*
       01  w-def-wcp.
      *        *-------------------------------------------------------*
      *        * Tipo movimento                                        *
      *        *-------------------------------------------------------*
           05  w-def-wcp-tmo-cdp          pic  x(05)   value "PC"     .
      *        *-------------------------------------------------------*
      *        * Parametri commessa                                    *
      *        *-------------------------------------------------------*
           05  w-def-wcp-dsz-prd          pic  9(02)   value 02       .
           05  w-def-wcp-esz-prd          pic  9(02)   value 01       .
           05  w-def-wcp-esz-cod          pic  9(07)   value zero     .
           05  w-def-wcp-esz-dpz          pic  x(04)   value spaces   .
           05  w-def-wcp-pri-prd          pic  x(02)   value spaces   .
           05  w-def-wcp-tip-mag          pic  9(02)   value 01       .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

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
      *    * Area di comunicazione per gestione commesse di produzione *
      *    *-----------------------------------------------------------*
           copy      "pgm/cdp/prg/cpy/mgescdp0.mdl"                   .

      ******************************************************************
       Procedure Division                using m-ges-cdp              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   m-ges-cdp-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        m-ges-cdp-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   m-ges-cdp-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   m-ges-cdp-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Generazione file                            *
      *                  *---------------------------------------------*
           else if   m-ges-cdp-tip-ope    =    "WC"
                     perform wcp-000      thru wcp-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       opn-100.
      *              *-------------------------------------------------*
      *              * [cdp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcdp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
      *              *-------------------------------------------------*
      *              * [cpx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpx                 .
      *              *-------------------------------------------------*
      *              * [cdpprt]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *              *-------------------------------------------------*
      *              * [cdpndo]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
      *              *-------------------------------------------------*
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * [cdp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcdp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
      *              *-------------------------------------------------*
      *              * [cpx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpx                 .
      *              *-------------------------------------------------*
      *              * [cdpprt]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *              *-------------------------------------------------*
      *              * [cdpndo]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   m-ges-cdp-exi-sts
           else      move  "#"            to   m-ges-cdp-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Scrittura commessa di produzione                          *
      *    *-----------------------------------------------------------*
       wcp-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su valori in input                     *
      *                  *---------------------------------------------*
           if        m-ges-cdp-num-pro    =    zero or
                     m-ges-cdp-qta-ord    =    zero
                     go to wcp-900.
       wcp-030.
      *                  *---------------------------------------------*
      *                  * Test su distinta base                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *                  *---------------------------------------------*
      *                  * Lettura [lgt]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TNMASS"             to   f-key                  .
           move      w-def-wcp-tip-mag    to   rf-lgt-tpm-ass         .
           move      m-ges-cdp-num-pro    to   rf-lgt-nrm-ass         .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *                  *---------------------------------------------*
      *                  * Se non esiste la distinta base per il codi- *
      *                  * ce prodotto, ad uscita                      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to wcp-900.
       wcp-050.
      *              *-------------------------------------------------*
      *              * Attribuzione numero protocollo commessa         *
      *              *-------------------------------------------------*
           move      m-ges-cdp-dat-orc    to   w-att-cdp-prt-dat      .
           move      m-ges-cdp-cod-dpz    to   w-att-cdp-prt-dpz      .
           perform   att-cdp-prt-000      thru att-cdp-prt-999        .
           move      w-att-cdp-prt-num    to   w-aux-wcp-num-prt      .
      *              *-------------------------------------------------*
      *              * Attribuzione numero documento commessa          *
      *              *-------------------------------------------------*
           move      zero                 to   w-att-cdp-ndo-num      .
           move      m-ges-cdp-dat-orc    to   w-att-cdp-ndo-dat      .
           move      m-ges-cdp-cod-dpz    to   w-att-cdp-ndo-dpz      .
           move      spaces               to   w-att-cdp-ndo-sgl      .
           perform   att-cdp-ndo-000      thru att-cdp-ndo-999        .
           move      w-att-cdp-ndo-num    to   w-aux-wcp-num-doc      .
       wcp-100.
      *              *-------------------------------------------------*
      *              * Letture complementari                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcp]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Lettura [dcp]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      m-ges-cdp-num-pro    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       wcp-400.
      *              *-------------------------------------------------*
      *              * Scrittura record [cdp]                          *
      *              *-------------------------------------------------*
           perform   wrt-rec-cdp-000      thru wrt-rec-cdp-999        .
       wcp-500.
      *              *-------------------------------------------------*
      *              * Scrittura record [cpx]                          *
      *              *-------------------------------------------------*
           perform   wrt-rec-cpx-000      thru wrt-rec-cpx-999        .
       wcp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     wcp-999.
       wcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero protocollo commessa di     *
      *    * produzione                                                *
      *    *-----------------------------------------------------------*
       att-cdp-prt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *              *-------------------------------------------------*
      *              * Preparazione chiave per lettura tabella numera- *
      *              * zioni [cdpprt]                                  *
      *              *-------------------------------------------------*
           move      w-att-cdp-prt-dat    to   s-dat                  .
           move      s-saa                to   w-att-cdp-prt-saa      .
           move      w-att-cdp-prt-saa    to   rn-cdp-prt-scl-ann     .
           move      w-att-cdp-prt-dpz    to   rn-cdp-prt-cod-dpz     .
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [cdpprt]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-cdp-prt-100.
      *                  *---------------------------------------------*
      *                  * Record non trovato                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione record normalizzato        *
      *                      *-----------------------------------------*
           move      zero                 to   rn-cdp-prt-num-prt     .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     att-cdp-prt-000.
       att-cdp-prt-100.
      *                  *---------------------------------------------*
      *                  * Record trovato                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero protocollo in work di comodo     *
      *                      *-----------------------------------------*
           move      rn-cdp-prt-num-prt   to   w-att-cdp-prt-wnu      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero protocollo         *
      *                      *-----------------------------------------*
           add       1                    to   w-att-cdp-prt-wpr      .
           if        w-att-cdp-prt-wpr    =    zero
                     move  1              to   w-att-cdp-prt-wpr      .
      *                      *-----------------------------------------*
      *                      * Forzatura secolo/anno                   *
      *                      *-----------------------------------------*
           move      w-att-cdp-prt-saa    to   w-att-cdp-prt-wsa      .
      *                      *-----------------------------------------*
      *                      * Forzatura dipendenza                    *
      *                      *-----------------------------------------*
           move      w-att-cdp-prt-dpz    to   w-att-cdp-prt-wdz      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione numero protocollo       *
      *                      *-----------------------------------------*
           move      w-att-cdp-prt-wnu    to   rn-cdp-prt-num-prt     .
      *                      *-----------------------------------------*
      *                      * Update record                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-cdp-prt-000.
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-prt             .
      *                      *-----------------------------------------*
      *                      * Valore in uscita                        *
      *                      *-----------------------------------------*
           move      rn-cdp-prt-num-prt   to   w-att-cdp-prt-num      .
       att-cdp-prt-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero documento commesse di pro- *
      *    * duzione                                                   *
      *    *-----------------------------------------------------------*
       att-cdp-ndo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-att-cdp-ndo-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
      *              *-------------------------------------------------*
      *              * Composizione chiave per lettura                 *
      *              *-------------------------------------------------*
           move      w-att-cdp-ndo-dat    to   s-dat                  .
           move      s-saa                to   w-att-cdp-ndo-saa      .
           move      w-att-cdp-ndo-saa    to   rn-cdp-ndo-scl-ann     .
           move      w-att-cdp-ndo-dpz    to   rn-cdp-ndo-cod-dpz     .
           move      w-att-cdp-ndo-sgl    to   rn-cdp-ndo-sgl-num     .
      *              *-------------------------------------------------*
      *              * Lettura numerazione                             *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
           if        f-sts                =    e-not-err
                     go to att-cdp-ndo-100.
      *              *-------------------------------------------------*
      *              * Se numerazione non esistente la si crea norma-  *
      *              * lizzata a zero                                  *
      *              *-------------------------------------------------*
           move      zero                 to   rn-cdp-ndo-num-doc     .
           move      zero                 to   rn-cdp-ndo-dat-doc     .
           move      "PT"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
           move      "RL"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
           go to     att-cdp-ndo-000.
       att-cdp-ndo-100.
      *              *-------------------------------------------------*
      *              * Se numerazione esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori originali            *
      *                  *---------------------------------------------*
           move      rn-cdp-ndo-num-doc   to   w-att-cdp-ndo-nsv      .
           move      rn-cdp-ndo-dat-doc   to   w-att-cdp-ndo-dsv      .
       att-cdp-ndo-200.
      *                  *---------------------------------------------*
      *                  * Se numero documento passato a zero          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore record in comodo ridefinito      *
      *                      *-----------------------------------------*
           move      rn-cdp-ndo-num-doc   to   w-att-cdp-ndo-wnu      .
      *                      *-----------------------------------------*
      *                      * Incremento numero progressivo del comodo*
      *                      * ridefinito                              *
      *                      *-----------------------------------------*
           add       1                    to   w-att-cdp-ndo-wpr      .
           if        w-att-cdp-ndo-wpr    =    zero
                     move  1              to   w-att-cdp-ndo-wpr      .
      *                      *-----------------------------------------*
      *                      * Forzatura secolo/anno                   *
      *                      *-----------------------------------------*
           move      w-att-cdp-ndo-saa    to   w-att-cdp-ndo-wsa      .
      *                      *-----------------------------------------*
      *                      * Forzatura dipendenza                    *
      *                      *-----------------------------------------*
           move      w-att-cdp-ndo-dpz    to   w-att-cdp-ndo-wdz      .
      *                      *-----------------------------------------*
      *                      * Movimento da comodo ridefinito a record *
      *                      *-----------------------------------------*
           move      w-att-cdp-ndo-wnu    to   rn-cdp-ndo-num-doc     .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      w-att-cdp-ndo-dat    to   rn-cdp-ndo-dat-doc     .
      *                      *-----------------------------------------*
      *                      * Update record                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-cdp-ndo-000.
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cdp/num/ioc/obj/incdpndo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-cdp-ndo             .
      *                      *-----------------------------------------*
      *                      * Salvataggio nuovo valore                *
      *                      *-----------------------------------------*
           move      rn-cdp-ndo-num-doc   to   w-att-cdp-ndo-new      .
      *                      *-----------------------------------------*
      *                      * Valore in uscita                        *
      *                      *-----------------------------------------*
           move      rn-cdp-ndo-num-doc   to   w-att-cdp-ndo-num      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     att-cdp-ndo-999.
       att-cdp-ndo-300.
      *                  *---------------------------------------------*
      *                  * Se numero documento passato non a zero      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     att-cdp-ndo-999.
       att-cdp-ndo-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [cdp]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-cdp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcdp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-cdp-ide-dat         .
           move      s-ute                to   rf-cdp-ide-ute         .
           move      s-fas                to   rf-cdp-ide-fas         .
      *
           move      m-ges-cdp-cod-dpz    to   rf-cdp-cod-dpz         .
           move      w-aux-wcp-num-prt    to   rf-cdp-num-prt         .
           move      w-def-wcp-tmo-cdp    to   rf-cdp-tmo-cdp         .
           move      m-ges-cdp-dat-orc    to   rf-cdp-dat-doc         .
           move      w-aux-wcp-num-doc    to   rf-cdp-num-doc         .
           move      m-ges-cdp-dat-orc    to   s-dat                  .
           move      s-saa                to   rf-cdp-scl-ann         .
           move      spaces               to   rf-cdp-sgl-num         .
      *
           move      w-def-wcp-dsz-prd    to   rf-cdp-dsz-prd         .
           move      m-ges-cdp-cod-cli    to   rf-cdp-dsz-cod         .
           move      m-ges-cdp-dpz-cli    to   rf-cdp-dsz-dpz         .
           move      w-def-wcp-esz-prd    to   rf-cdp-esz-prd         .
           move      w-def-wcp-esz-cod    to   rf-cdp-esz-cod         .
           move      w-def-wcp-esz-dpz    to   rf-cdp-esz-dpz         .
           move      m-ges-cdp-dcn-prv    to   rf-cdp-dat-cns         .
           move      w-def-wcp-pri-prd    to   rf-cdp-pri-prd         .
           move      spaces               to   rf-cdp-com-int         .
      *
           move      spaces               to   rf-cdp-voc-des (1)     .
           move      spaces               to   rf-cdp-voc-des (2)     .
           move      spaces               to   rf-cdp-voc-des (3)     .
           move      spaces               to   rf-cdp-voc-des (4)     .
           move      spaces               to   rf-cdp-voc-des (5)     .
           move      spaces               to   rf-cdp-voc-des (6)     .
      *
           move      w-def-wcp-tip-mag    to   rf-cdp-tip-mag         .
           move      m-ges-cdp-num-pro    to   rf-cdp-num-mag         .
           move      rf-dcp-alf-pro       to   rf-cdp-alf-mag         .
           move      spaces               to   rf-cdp-sgl-vrn         .
           move      rf-dcp-umi-ven       to   rf-cdp-umi-prd         .
           move      rf-dcp-dec-qta       to   rf-cdp-dec-qta         .
           move      m-ges-cdp-qta-ord    to   rf-cdp-qta-dap         .
      *
           move      spaces               to   rf-cdp-cod-dsl         .
           move      zero                 to   rf-cdp-snx-2qt         .
           move      zero                 to   rf-cdp-snx-2qt         .
           move      zero                 to   rf-cdp-dec-2qt         .
           move      zero                 to   rf-cdp-qta-a02         .
           move      zero                 to   rf-cdp-snx-3qt         .
           move      zero                 to   rf-cdp-dec-3qt         .
           move      zero                 to   rf-cdp-qta-a03         .
           move      zero                 to   rf-cdp-ctr-stp         .
           move      spaces               to   rf-cdp-sdc-ccs         .
           move      spaces               to   rf-cdp-flg-cch         .
           move      spaces               to   rf-cdp-flg-ela         .
           move      spaces               to   rf-cdp-flg-pul         .
           move      spaces               to   rf-cdp-alx-exp         .
       cmp-rec-cdp-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [cdp]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-cdp-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-cdp-000      thru cmp-rec-cdp-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcdp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
       wrt-rec-cdp-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [cpx]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-cpx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-aux-wcp-num-prt    to   rf-cpx-num-prt         .
           move      m-ges-cdp-prt-orc    to   rf-cpx-prt-orc         .
           move      m-ges-cdp-prg-orc    to   rf-cpx-prg-orc         .
           move      spaces               to   rf-cpx-alx-exp         .
       cmp-rec-cpx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [cdp]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-cpx-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-cpx-000      thru cmp-rec-cpx-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofcpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpx                 .
       wrt-rec-cpx-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


