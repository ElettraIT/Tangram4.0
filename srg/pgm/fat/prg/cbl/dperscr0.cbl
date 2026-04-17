       Identification Division.
       Program-Id.                                 dperscr0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:                        *
      *                                   Fase:    dperscr0            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/10/00    *
      *                       Ultima revisione:    NdK del 17/07/11    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione degli sconti in riga              *
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
      *        Input  : d-per-scr-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-per-scr-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-per-scr-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-per-scr-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "SR" - Determinazione sconti in riga                           *
      *                                                                *
      *                                                                *
      *        Input  : d-per-scr-tip-ope = "SR"                       *
      *                                                                *
      *                 d-per-scr-psr-pro = % di sconto in riga asso-  *
      *                                     ciate al prodotto          *
      *                                                                *
      *                 d-per-scr-csr-pro = Cat. sconto in riga asso-  *
      *                                     ciata al prodotto          *
      *                                                                *
      *                 d-per-scr-psr-cli = % di sconto in riga asso-  *
      *                                     ciate al cliente           *
      *                                                                *
      *                 d-per-scr-csr-cli = Cat. sconto in riga asso-  *
      *                                     ciata al cliente           *
      *                                                                *
      *                 d-per-scr-cod-cli = Codice cliente             *
      *                                                                *
      *                 d-per-scr-cod-lst = Codice listino             *
      *                                                                *
      *                 d-per-scr-sgl-vpf = Sigla valuta               *
      *                                                                *
      *                 d-per-scr-num-pro = Codice numerico magazzino  *
      *                                                                *
      *                 d-per-scr-dat-rif = Data riferimento per de-   *
      *                                     terminazione sconti        *
      *                                                                *
      *        Output : d-per-scr-per-scr = Sconti in riga             *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zsx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsx"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento percentuali di sconto in riga      *
      *        *-------------------------------------------------------*
           05  w-prs-tfu-psr.
      *            *---------------------------------------------------*
      *            * Numero elementi in tabella                        *
      *            *---------------------------------------------------*
               10  w-prs-tfu-psr-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero massimo elementi in tabella                *
      *            *---------------------------------------------------*
               10  w-prs-tfu-psr-max      pic  9(03) value 15         .
      *            *---------------------------------------------------*
      *            * Tabella tipi di calcolo                           *
      *            *---------------------------------------------------*
               10  w-prs-tfu-psr-tbl occurs 15.
      *                *-----------------------------------------------*
      *                * Codice di calcolo                             *
      *                *-----------------------------------------------*
                   15  w-prs-tfu-psr-cdc  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Segnale di continuazione con calcolo succes-  *
      *                * sivo                                          *
      *                *-----------------------------------------------*
                   15  w-prs-tfu-psr-sdc  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work-area di comodo                               *
      *            *---------------------------------------------------*
               10  w-prs-tfu-psr-wst.
                   15  w-prs-tfu-psr-wel occurs 15.
                       20  w-prs-tfu-psr-aaa
                                          pic  9(02)                  .
                       20  w-prs-tfu-psr-bbb
                                          pic  x(01)                  .
               10  w-prs-tfu-psr-wcc      pic  9(02)                  .
               10  w-prs-tfu-psr-wsc      pic  x(01)                  .
               10  w-prs-tfu-psr-c01      pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work area per determinazioni varie                        *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work area di appoggio alla routine di determinazione  *
      *        *-------------------------------------------------------*
           05  w-det-scr.
      *            *---------------------------------------------------*
      *            * Comodo per indice su ultimo sconto significativo  *
      *            * determinato                                       *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-ius      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodo per codice di calcolo in esame             *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-cdc      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodo per segnale di continuazione con il calco- *
      *            * lo successivo in esame                            *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-sdc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di determinazione calcolo effettuata         *
      *            * - Spaces : Non effettuata                         *
      *            * - #      : Effettuata                             *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-fde      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di significativita' determinazione effettua- *
      *            * ta                                                *
      *            * - Spaces : Non significativo                      *
      *            * - #      : Significativo                          *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-fsd      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Comodo per % di sconto per aggiornamento          *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-psa occurs 05
                                          pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-scr-scr-c01      pic  9(02)                  .
               10  w-det-scr-scr-c02      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione sconti in riga   *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dperscr0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-per-scr              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-per-scr-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-per-scr-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-per-scr-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-per-scr-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di vendita            *
      *                  *---------------------------------------------*
           else if   d-per-scr-tip-ope    =    "SR"
                     perform dsr-000      thru dsr-999                .
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
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento percentuali di sconto in *
      *                  * riga                                        *
      *                  *---------------------------------------------*
           perform   prs-tfu-psr-000      thru prs-tfu-psr-999        .
       opn-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [lst]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * [lsd]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * [zcs]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * [zsx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsx                 .
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
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [lst]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * [lsd]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * [zcs]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * [zsx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsx                 .
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
                     move  spaces         to   d-per-scr-exi-sts
           else      move  "#"            to   d-per-scr-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di sconto in riga              *
      *    *-----------------------------------------------------------*
       dsr-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-per-scr-per-scr (1)  .
           move      zero                 to   d-per-scr-per-scr (2)  .
           move      zero                 to   d-per-scr-per-scr (3)  .
           move      zero                 to   d-per-scr-per-scr (4)  .
           move      zero                 to   d-per-scr-per-scr (5)  .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice ultimo sconto signifi-  *
      *              * cativo                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-scr-scr-ius      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-scr-scr-c01      .
       dsr-050.
           add       1                    to   w-det-scr-scr-c01      .
           if        w-det-scr-scr-c01    >    w-prs-tfu-psr-ele
                     go to dsr-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di determinazione effet-  *
      *              * tuata a : No                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-scr-scr-fde      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di significativita' de-   *
      *              * terminazione effettuata a : No                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-scr-scr-fsd      .
      *              *-------------------------------------------------*
      *              * Codice di calcolo in work di comodo             *
      *              *-------------------------------------------------*
           move      w-prs-tfu-psr-cdc
                    (w-det-scr-scr-c01)   to   w-det-scr-scr-cdc      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del codice di calcolo    *
      *              *-------------------------------------------------*
           if        w-det-scr-scr-cdc    =    00
                     go to dsr-100
           else if   w-det-scr-scr-cdc    =    21
                     go to dsr-210
           else if   w-det-scr-scr-cdc    =    22
                     go to dsr-220
           else if   w-det-scr-scr-cdc    =    31
                     go to dsr-310
           else if   w-det-scr-scr-cdc    =    32
                     go to dsr-320
           else if   w-det-scr-scr-cdc    =    41
                     go to dsr-410
           else if   w-det-scr-scr-cdc    =    51
                     go to dsr-510
           else if   w-det-scr-scr-cdc    =    52
                     go to dsr-520
           else if   w-det-scr-scr-cdc    =    61
                     go to dsr-610
           else if   w-det-scr-scr-cdc    =    71
                     go to dsr-710
           else if   w-det-scr-scr-cdc    =    72
                     go to dsr-720
           else if   w-det-scr-scr-cdc    =    81
                     go to dsr-810.
       dsr-100.
      *              *=================================================*
      *              * Codice di calcolo : Nessun calcolo              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-210.
      *              *=================================================*
      *              * Codice di calcolo : % dirette da anagrafica     *
      *              * prodotto                                        *
      *              *-------------------------------------------------*
       dsr-212.
      *                  *---------------------------------------------*
      *                  * Tentativo di determinazione da listino sto- *
      *                  * rico                                        *
      *                  *                                             *
      *                  * Attenzione : questa operazione risultera'   *
      *                  *              funzionale solo in presenza di *
      *                  *              programmi su misura di stori-  *
      *                  *              cizzazione, relativi al solo   *
      *                  *              listino base, che forzino al   *
      *                  *              valore 'S' il flag di signifi- *
      *                  *              cativita' degli sconti         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      01                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lsd-num-pro         .
           move      d-per-scr-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-216.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-216.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 01                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = c-sgl             or
                     rf-lsd-num-pro       not  = d-per-scr-num-pro
                     go to dsr-216.
      *                      *-----------------------------------------*
      *                      * Test sul flag di significativita' de-   *
      *                      * gli sconti contenuti nel record         *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-sco       not  = "S"
                     go to dsr-216.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazio-  *
      *                      * ne effettuata a : Si                    *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-lsd-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lsd-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lsd-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lsd-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lsd-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte signi-   *
      *                      * ficative : uscita immediata             *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dsr-900.
       dsr-216.
      *                  *---------------------------------------------*
      *                  * Determinazione da [dcp], dai dati in input  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a : Si*
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte a zero : *
      *                      * a controllo finale                      *
      *                      *-----------------------------------------*
           if        d-per-scr-psr-pro (1)
                                          =    zero and
                     d-per-scr-psr-pro (2)
                                          =    zero and
                     d-per-scr-psr-pro (3)
                                          =    zero and
                     d-per-scr-psr-pro (4)
                                          =    zero and
                     d-per-scr-psr-pro (5)
                                          =    zero
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazio-  *
      *                      * ne effettuata a : Si                    *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      d-per-scr-psr-pro (1)
                                          to   w-det-scr-scr-psa (1)  .
           move      d-per-scr-psr-pro (2)
                                          to   w-det-scr-scr-psa (2)  .
           move      d-per-scr-psr-pro (3)
                                          to   w-det-scr-scr-psa (3)  .
           move      d-per-scr-psr-pro (4)
                                          to   w-det-scr-scr-psa (4)  .
           move      d-per-scr-psr-pro (5)
                                          to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte signi-   *
      *                      * ficative : uscita immediata             *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dsr-900.
       dsr-220.
      *              *=================================================*
      *              * Codice di calcolo : % indirette da categoria    *
      *              * di sconto in riga in anagrafica prodotto        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se categoria di sconto associata al prodot- *
      *                  * to a zero : A controllo finale              *
      *                  *---------------------------------------------*
           if        d-per-scr-csr-pro    =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zcs]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      03                   to   rf-zcs-tip-csc         .
           move      d-per-scr-csr-pro    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto in riga tutte a    *
      *                  * zero : A controllo finale                   *
      *                  *---------------------------------------------*
           if        rf-zcs-per-sco (1)   =    zero and
                     rf-zcs-per-sco (2)   =    zero and
                     rf-zcs-per-sco (3)   =    zero and
                     rf-zcs-per-sco (4)   =    zero and
                     rf-zcs-per-sco (5)   =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      rf-zcs-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-zcs-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-zcs-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-zcs-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-zcs-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-310.
      *              *=================================================*
      *              * Codice di calcolo : % dirette da anagrafica     *
      *              * commerciale cliente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte a zero : A   *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-per-scr-psr-cli (1)
                                          =    zero and
                     d-per-scr-psr-cli (2)
                                          =    zero and
                     d-per-scr-psr-cli (3)
                                          =    zero and
                     d-per-scr-psr-cli (4)
                                          =    zero and
                     d-per-scr-psr-cli (5)
                                          =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      d-per-scr-psr-cli (1)
                                          to   w-det-scr-scr-psa (1)  .
           move      d-per-scr-psr-cli (2)
                                          to   w-det-scr-scr-psa (2)  .
           move      d-per-scr-psr-cli (3)
                                          to   w-det-scr-scr-psa (3)  .
           move      d-per-scr-psr-cli (4)
                                          to   w-det-scr-scr-psa (4)  .
           move      d-per-scr-psr-cli (5)
                                          to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-320.
      *              *=================================================*
      *              * Codice di calcolo : % indirette da categoria    *
      *              * di sconto in anagrafica commerciale cliente     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se categoria di sconto associata al cliente *
      *                  * a zero : A controllo finale                 *
      *                  *---------------------------------------------*
           if        d-per-scr-csr-cli    =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zcs]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      02                   to   rf-zcs-tip-csc         .
           move      d-per-scr-csr-cli    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto in riga tutte a    *
      *                  * zero : A controllo finale                   *
      *                  *---------------------------------------------*
           if        rf-zcs-per-sco (1)   =    zero and
                     rf-zcs-per-sco (2)   =    zero and
                     rf-zcs-per-sco (3)   =    zero and
                     rf-zcs-per-sco (4)   =    zero and
                     rf-zcs-per-sco (5)   =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      rf-zcs-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-zcs-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-zcs-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-zcs-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-zcs-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-410.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi netti concordati per cliente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [lst]                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      02                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      d-per-scr-cod-cli    to   rf-lst-cod-cli         .
           move      d-per-scr-sgl-vpf    to   rf-lst-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Se Si/No utilizzo delle % di sconto in riga *
      *                  * a No : A controllo finale                   *
      *                  *---------------------------------------------*
           if        rf-lst-snx-sco       not  = "S"
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Test su data di inizio validita'            *
      *                  *---------------------------------------------*
           if        rf-lst-dva-ini       =    zero
                     go to dsr-470.
           if        rf-lst-dva-ini       >    d-per-scr-dat-rif
                     go to dsr-900.
       dsr-470.
      *                  *---------------------------------------------*
      *                  * Test su data di fine validita'              *
      *                  *---------------------------------------------*
           if        rf-lst-dva-fin       =    zero
                     go to dsr-480.
           if        rf-lst-dva-fin       <    d-per-scr-dat-rif
                     go to dsr-900.
       dsr-480.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      rf-lst-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lst-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lst-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lst-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lst-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-510.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi per campagna di vendita                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino per campa- *
      *                  * gna di vendita attuale                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-515.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       >    d-per-scr-dat-rif
                     go to dsr-515.
      *                      *-----------------------------------------*
      *                      * Test su data di fine campagna           *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       <    d-per-scr-dat-rif
                     go to dsr-515.
      *                      *-----------------------------------------*
      *                      * Se Si/No utilizzo delle % di sconto in  *
      *                      * riga a No : A controllo finale          *
      *                      *-----------------------------------------*
           if        rf-lst-snx-sco       not  = "S"
                     go to dsr-515.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-lst-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lst-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lst-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lst-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lst-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte signi-   *
      *                      * ficative : uscita immediata             *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dsr-900.
       dsr-515.
      *                  *---------------------------------------------*
      *                  * Determinazione sconti per campagna di ven-  *
      *                  * dita storico                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lsd-num-pro         .
           move      d-per-scr-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 03                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = c-sgl             or
                     rf-lsd-num-pro       not  = d-per-scr-num-pro
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    d-per-scr-dat-rif
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Se Si/No utilizzo delle % di sconto in  *
      *                      * riga a No : A controllo finale          *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-sco       not  = "S"
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-lsd-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lsd-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lsd-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lsd-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lsd-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte signi-   *
      *                      * ficative : uscita immediata             *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dsr-900.
       dsr-520.
      *              *=================================================*
      *              * Codice di calcolo : % indirette contenute in    *
      *              * categoria sconto in campagna                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   dsr-052-000          thru dsr-052-999            .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significati- *
      *                  * ve : uscita immediata                       *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-610.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi di listino di vendita                    *
      *              *                                                 *
      *              * N.B.: per il listino Base non viene fatta la    *
      *              *       determinazione in quanto la % e' legata   *
      *              *       all'anagrafica prodotto e di conseguenza  *
      *              *       dovrebbe essere gia' specificato il tipo  *
      *              *       di calcolo '21'                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione sconti di listino di vendita *
      *                  * storico                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se listino base : oltre                 *
      *                      *-----------------------------------------*
           if        d-per-scr-cod-lst    =    spaces
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      01                   to   rf-lsd-tip-rec         .
           move      d-per-scr-cod-lst    to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      d-per-scr-sgl-vpf    to   rf-lsd-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lsd-num-pro         .
           move      d-per-scr-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-615.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-615.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 01                or
                     rf-lsd-cod-lst       not  = d-per-scr-cod-lst or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = d-per-scr-sgl-vpf or
                     rf-lsd-num-pro       not  = d-per-scr-num-pro
                     go to dsr-615.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio validita'        *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    d-per-scr-dat-rif
                     go to dsr-615.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-lsd-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lsd-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lsd-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lsd-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lsd-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte signi-   *
      *                      * ficative : uscita immediata             *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dsr-900.
       dsr-615.
      *                  *---------------------------------------------*
      *                  * Determinazione sconti di listino di vendita *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      01                   to   rf-lst-tip-rec         .
           move      d-per-scr-cod-lst    to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      d-per-scr-sgl-vpf    to   rf-lst-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-lst-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lst-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lst-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lst-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lst-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto tutte signi-   *
      *                      * ficative : uscita immediata             *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dsr-900.
       dsr-710.
      *              *=================================================*
      *              * Codice di calcolo : % indirette per incrocio    *
      *              * tra :                                           *
      *              *                                                 *
      *              * - cliente                                       *
      *              * - categoria di sconto in riga contenuta in ana- *
      *              *   grafica prodotto                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se categoria di sconto associata al prodot- *
      *                  * to a zero : A controllo finale              *
      *                  *---------------------------------------------*
           if        d-per-scr-csr-pro    =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zcs]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      03                   to   rf-zcs-tip-csc         .
           move      d-per-scr-csr-pro    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zsx]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UNODUE    "         to   f-key                  .
           move      02                   to   rf-zsx-tip-inc         .
           move      d-per-scr-cod-cli    to   rf-zsx-cod-uno         .
           move      d-per-scr-csr-pro    to   rf-zsx-cod-due         .
           move      "pgm/dcc/fls/ioc/obj/iofzsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsx                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto in riga tutte a    *
      *                  * zero : A controllo finale                   *
      *                  *---------------------------------------------*
           if        rf-zsx-per-sco (1)   =    zero and
                     rf-zsx-per-sco (2)   =    zero and
                     rf-zsx-per-sco (3)   =    zero and
                     rf-zsx-per-sco (4)   =    zero and
                     rf-zsx-per-sco (5)   =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      rf-zsx-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-zsx-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-zsx-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-zsx-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-zsx-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-720.
      *              *=================================================*
      *              * Codice di calcolo : % indirette per incrocio    *
      *              * tra :                                           *
      *              * - categoria di sconto in riga contenuta in ana- *
      *              *   grafica commerciale cliente                   *
      *              * - categoria di sconto in riga contenuta in ana- *
      *              *   grafica prodotto                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se categoria di sconto associata al cliente *
      *                  * a zero : A controllo finale                 *
      *                  *---------------------------------------------*
           if        d-per-scr-csr-cli    =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zcs]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      02                   to   rf-zcs-tip-csc         .
           move      d-per-scr-csr-cli    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Se categoria di sconto associata al prodot- *
      *                  * to a zero : A controllo finale              *
      *                  *---------------------------------------------*
           if        d-per-scr-csr-pro    =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zcs]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      03                   to   rf-zcs-tip-csc         .
           move      d-per-scr-csr-pro    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Lettura record [zsx]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UNODUE    "         to   f-key                  .
           move      01                   to   rf-zsx-tip-inc         .
           move      d-per-scr-csr-cli    to   rf-zsx-cod-uno         .
           move      d-per-scr-csr-pro    to   rf-zsx-cod-due         .
           move      "pgm/dcc/fls/ioc/obj/iofzsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsx                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto in riga tutte a    *
      *                  * zero : A controllo finale                   *
      *                  *---------------------------------------------*
           if        rf-zsx-per-sco (1)   =    zero and
                     rf-zsx-per-sco (2)   =    zero and
                     rf-zsx-per-sco (3)   =    zero and
                     rf-zsx-per-sco (4)   =    zero and
                     rf-zsx-per-sco (5)   =    zero
                     go to dsr-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      rf-zsx-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-zsx-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-zsx-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-zsx-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-zsx-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-810.
      *              *=================================================*
      *              * Codice di calcolo : % indirette da listini di   *
      *              *                     vendita a scaglioni         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   dsr-081-000          thru dsr-081-999            .
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto tutte significa-   *
      *                  * tive : uscita immediata                     *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-ius    =    5
                     go to dsr-999.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dsr-900.
       dsr-900.
      *              *=================================================*
      *              * Controllo finale                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione con il calcolo     *
      *                  * successivo in work di comodo                *
      *                  *---------------------------------------------*
           move      w-prs-tfu-psr-sdc
                    (w-det-scr-scr-c01)   to   w-det-scr-scr-sdc      .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore del se-   *
      *                  * gnale di continuazione                      *
      *                  *---------------------------------------------*
           if        w-det-scr-scr-sdc    =    " "
                     go to dsr-910
           else if   w-det-scr-scr-sdc    =    "+"
                     go to dsr-920
           else if   w-det-scr-scr-sdc    =    "."
                     go to dsr-930
           else if   w-det-scr-scr-sdc    =    "#"
                     go to dsr-940
           else if   w-det-scr-scr-sdc    =    "z"
                     go to dsr-950.
       dsr-910.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Interrompi subi- *
      *                  * to il calcolo                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dsr-999.
       dsr-920.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua comun-  *
      *                  * que con il calcolo successivo               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     dsr-050.
       dsr-930.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se il calcolo  *
      *                  * in esame non ha prodotto una determinazione *
      *                  * di sconti, altrimenti interrompi            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non effettuata : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-fde    =    spaces
                     go to dsr-050.
      *                      *-----------------------------------------*
      *                      * Se determinazione non significativa :   *
      *                      * riciclo                                 *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-fsd    =    spaces
                     go to dsr-050.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dsr-999.
       dsr-940.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se se non e'   *
      *                  * stata trovata la tabella di riferimento,    *
      *                  * altrimenti interrompi                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non effettuata : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-fde    =    spaces
                     go to dsr-050.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dsr-999.
       dsr-950.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se le percen-  *
      *                  * tuali di sconto sinora cumulate sono pari a *
      *                  * zero, altrimenti interrompi                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se indice su ultimo sconto signi-  *
      *                      * ficativo a zero                         *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    =    zero
                     go to dsr-050.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dsr-999.
       dsr-980.
      *              *=================================================*
      *              * Subroutine per aggiornamento % di sconto        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo per 5 percentuali                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-scr-scr-c02      .
       dsr-982.
           add       1                    to   w-det-scr-scr-c02      .
           if        w-det-scr-scr-c02    >    5
                     go to dsr-989.
      *                      *-----------------------------------------*
      *                      * Se % per aggiornamento a zero : riciclo *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-psa
                    (w-det-scr-scr-c02)   =    zero
                     go to dsr-982.
      *                      *-----------------------------------------*
      *                      * Incremento indice su ultima % signifi-  *
      *                      * cativa                                  *
      *                      *-----------------------------------------*
           add       1                    to   w-det-scr-scr-ius      .
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo : a fine            *
      *                      *-----------------------------------------*
           if        w-det-scr-scr-ius    >    5
                     move  5              to   w-det-scr-scr-ius
                     go to dsr-989.
      *                      *-----------------------------------------*
      *                      * Memorizzazione % di sconto              *
      *                      *-----------------------------------------*
           move      w-det-scr-scr-psa
                    (w-det-scr-scr-c02)   to   d-per-scr-per-scr
                                              (w-det-scr-scr-ius)     .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     dsr-982.
       dsr-989.
           exit.
       dsr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di sconto in riga              *
      *    *                                                           *
      *    * Subroutine per il calcolo % di sconto indirette da cate-  *
      *    * goria in prezzi in campagna di vendita                    *
      *    *                                                           *
      *    * Tipo calcolo : 52                                         *
      *    *-----------------------------------------------------------*
       dsr-052-000.
      *              *-------------------------------------------------*
      *              * Prima fase                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino per campa- *
      *                  * gna di vendita attuale                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-052-500.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       >    d-per-scr-dat-rif
                     go to dsr-052-500.
      *                      *-----------------------------------------*
      *                      * Test su data di fine campagna           *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       <    d-per-scr-dat-rif
                     go to dsr-052-500.
      *                      *-----------------------------------------*
      *                      * Se Si/No utilizzo delle % di sconto in  *
      *                      * riga a No : oltre                       *
      *                      *-----------------------------------------*
           if        rf-lst-snx-sco       not  = "S"
                     go to dsr-052-500.
       dsr-052-200.
      *                      *-----------------------------------------*
      *                      * Se categoria di sconto associata alla   *
      *                      * campagna di vendita : oltre             *
      *                      *-----------------------------------------*
           if        rf-lst-cat-sco       =    zero
                     go to dsr-052-500.
      *                      *-----------------------------------------*
      *                      * Lettura record [zcs]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      03                   to   rf-zcs-tip-csc         .
           move      rf-lst-cat-sco       to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                          *-------------------------------------*
      *                          * Se lettura errata : oltre           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-052-500.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto in riga tutte  *
      *                      * zero : A controllo finale               *
      *                      *-----------------------------------------*
           if        rf-zcs-per-sco (1)   =    zero and
                     rf-zcs-per-sco (2)   =    zero and
                     rf-zcs-per-sco (3)   =    zero and
                     rf-zcs-per-sco (4)   =    zero and
                     rf-zcs-per-sco (5)   =    zero
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-zcs-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-zcs-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-zcs-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-zcs-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-zcs-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dsr-052-900.
       dsr-052-500.
      *              *-------------------------------------------------*
      *              * Seconda fase                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione sconti per campagna di ven-  *
      *                  * dita storico                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      d-per-scr-num-pro    to   rf-lsd-num-pro         .
           move      d-per-scr-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 03                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = c-sgl             or
                     rf-lsd-num-pro       not  = d-per-scr-num-pro
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    d-per-scr-dat-rif
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Se Si/No utilizzo delle % di sconto in  *
      *                      * riga a No : A controllo finale          *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-sco       not  = "S"
                     go to dsr-052-900.
       dsr-052-700.
      *                      *-----------------------------------------*
      *                      * Se categoria di sconto associata alla   *
      *                      * campagna di vendita : oltre             *
      *                      *-----------------------------------------*
           if        rf-lsd-cat-sco       =    zero
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Lettura record [zcs]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      03                   to   rf-zcs-tip-csc         .
           move      rf-lsd-cat-sco       to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *                          *-------------------------------------*
      *                          * Se lettura errata : oltre           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                      *-----------------------------------------*
      *                      * Se percentuali di sconto in riga tutte  *
      *                      * zero : A controllo finale               *
      *                      *-----------------------------------------*
           if        rf-zcs-per-sco (1)   =    zero and
                     rf-zcs-per-sco (2)   =    zero and
                     rf-zcs-per-sco (3)   =    zero and
                     rf-zcs-per-sco (4)   =    zero and
                     rf-zcs-per-sco (5)   =    zero
                     go to dsr-052-900.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di sconto     *
      *                      *-----------------------------------------*
           move      rf-zcs-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-zcs-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-zcs-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-zcs-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-zcs-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                      *-----------------------------------------*
      *                      * Subroutine di aggiornamento             *
      *                      *-----------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dsr-052-900.
       dsr-052-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsr-052-999.
       dsr-052-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di sconto in riga              *
      *    *                                                           *
      *    * Subroutine per il calcolo % di sconto indirette da listi- *
      *    * ni di vendita a scaglioni di quantita'                    *
      *    *                                                           *
      *    * Tipo calcolo : 81                                         *
      *    *                                                           *
      *    * N.B. : Attualmente non sono previsti storici per gli      *
      *    *        scaglioni                                          *
      *    *-----------------------------------------------------------*
       dsr-081-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su quantita' di riferimento            *
      *                  *---------------------------------------------*
           if        d-per-scr-qta-rif    =    zero
                     go to dsr-081-900.
       dsr-081-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [lst]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PROLST    "         to   f-key                  .
           move      81                   to   rf-lst-tip-rec         .
           move      d-per-scr-num-pro    to   rf-lst-num-pro         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      d-per-scr-cod-lst    to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-081-900.
       dsr-081-200.
      *              *-------------------------------------------------*
      *              * Read next [lst]                                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dsr-081-900.
       dsr-081-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-lst-tip-rec       not  = 81                or
                     rf-lst-num-pro       not  = d-per-scr-num-pro or
                     rf-lst-sgl-vlt       not  = c-sgl             or
                     rf-lst-cod-lst       not  = d-per-scr-cod-lst
                     go to dsr-081-900.
       dsr-081-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [lst]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto con la quantita' di riferimento   *
      *                  *---------------------------------------------*
           if        d-per-scr-qta-rif    not  > rf-lst-qta-rif
                     go to dsr-081-600
           else      go to dsr-081-200.
       dsr-081-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione sconti                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fde      .
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-scr-scr-fsd      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di sconto         *
      *                  *---------------------------------------------*
           move      rf-lst-per-sco (1)   to   w-det-scr-scr-psa (1)  .
           move      rf-lst-per-sco (2)   to   w-det-scr-scr-psa (2)  .
           move      rf-lst-per-sco (3)   to   w-det-scr-scr-psa (3)  .
           move      rf-lst-per-sco (4)   to   w-det-scr-scr-psa (4)  .
           move      rf-lst-per-sco (5)   to   w-det-scr-scr-psa (5)  .
      *                  *---------------------------------------------*
      *                  * Subroutine di aggiornamento                 *
      *                  *---------------------------------------------*
           perform   dsr-980              thru dsr-989                .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dsr-081-900.
       dsr-081-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsr-081-999.
       dsr-081-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo funzionamento percentua- *
      *    *                             li di sconto in riga          *
      *    *-----------------------------------------------------------*
       prs-tfu-psr-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero elementi in tabella     *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-tfu-psr-ele      .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tfu-psr]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to prs-tfu-psr-900.
      *              *-------------------------------------------------*
      *              * Valore personalizzazione in comodo ridefinito   *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-tfu-psr-wst      .
       prs-tfu-psr-100.
      *              *-------------------------------------------------*
      *              * Test se valore a spazi                          *
      *              *-------------------------------------------------*
           if        w-prs-tfu-psr-wst    =    spaces
                     go to prs-tfu-psr-900.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione stringa letta                *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-tfu-psr-c01      .
       prs-tfu-psr-120.
           add       1                    to   w-prs-tfu-psr-c01      .
           if        w-prs-tfu-psr-c01    >    w-prs-tfu-psr-max
                     go to prs-tfu-psr-900.
      *                  *---------------------------------------------*
      *                  * Controllo formale se valore ammissibile     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su codice di calcolo                    *
      *                      *-----------------------------------------*
           if        w-prs-tfu-psr-aaa
                    (w-prs-tfu-psr-c01)   not  numeric
                     go to prs-tfu-psr-120.
      *                  *---------------------------------------------*
      *                  * Codice di calcolo in work di comodo         *
      *                  *---------------------------------------------*
           move      w-prs-tfu-psr-aaa
                    (w-prs-tfu-psr-c01)   to   w-prs-tfu-psr-wcc      .
      *                  *---------------------------------------------*
      *                  * Controllo su valori ammessi                 *
      *                  *---------------------------------------------*
           if        w-prs-tfu-psr-wcc    not  = 00 and
                     w-prs-tfu-psr-wcc    not  = 21 and
                     w-prs-tfu-psr-wcc    not  = 22 and
                     w-prs-tfu-psr-wcc    not  = 31 and
                     w-prs-tfu-psr-wcc    not  = 32 and
                     w-prs-tfu-psr-wcc    not  = 41 and
                     w-prs-tfu-psr-wcc    not  = 51 and
                     w-prs-tfu-psr-wcc    not  = 52 and
                     w-prs-tfu-psr-wcc    not  = 61 and
                     w-prs-tfu-psr-wcc    not  = 71 and
                     w-prs-tfu-psr-wcc    not  = 72
                     go to prs-tfu-psr-120.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione calcolo in work di *
      *                  * comodo                                      *
      *                  *---------------------------------------------*
           move      w-prs-tfu-psr-bbb
                    (w-prs-tfu-psr-c01)   to   w-prs-tfu-psr-wsc      .
      *                  *---------------------------------------------*
      *                  * Controllo su valori ammessi                 *
      *                  *---------------------------------------------*
           if        w-prs-tfu-psr-wsc    not  = " " and
                     w-prs-tfu-psr-wsc    not  = "+" and
                     w-prs-tfu-psr-wsc    not  = "." and
                     w-prs-tfu-psr-wsc    not  = "#" and
                     w-prs-tfu-psr-wsc    not  = "z"
                     go to prs-tfu-psr-120.
      *                  *---------------------------------------------*
      *                  * Memorizzazione elemento in tabella          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contatore elementi bufferizzati *
      *                      *-----------------------------------------*
           if        w-prs-tfu-psr-ele    not  < w-prs-tfu-psr-max
                     go to prs-tfu-psr-900.
      *                      *-----------------------------------------*
      *                      * Incremento contatore elementi bufferiz- *
      *                      * zati                                    *
      *                      *-----------------------------------------*
           add       1                    to   w-prs-tfu-psr-ele      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione elemento                 *
      *                      *-----------------------------------------*
           move      w-prs-tfu-psr-wcc    to   w-prs-tfu-psr-cdc
                                              (w-prs-tfu-psr-ele)     .
           move      w-prs-tfu-psr-wsc    to   w-prs-tfu-psr-sdc
                                              (w-prs-tfu-psr-ele)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione stringa letta      *
      *                      *-----------------------------------------*
           go to     prs-tfu-psr-120.
       prs-tfu-psr-900.
      *              *-------------------------------------------------*
      *              * Test su numero elementi bufferizzati            *
      *              *-------------------------------------------------*
           if        w-prs-tfu-psr-ele    not  = zero
                     go to prs-tfu-psr-999.
      *              *-------------------------------------------------*
      *              * Se zero elementi bufferizzati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore di default                           *
      *                  *---------------------------------------------*
           move      "41.51.72.71.21.22.31.32 "
                                          to   w-prs-tfu-psr-wst      .
      *                  *---------------------------------------------*
      *                  * Riciclo a rielaborazione stringa            *
      *                  *---------------------------------------------*
           go to     prs-tfu-psr-100.
       prs-tfu-psr-999.
           exit.

