       Identification Division.
       Program-Id.                                 dprzacq0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    ffo                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/08/91    *
      *                       Ultima revisione:    NdK del 06/12/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione dei prezzi e sconti di acquisto   *
      * per fornitore                                                  *
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
      *        Input  : d-prz-acq-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-acq-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-acq-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-prz-acq-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "PA" - Determinazione prezzi e sconti di acquisto              *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-acq-tip-ope = "PA"                       *
      *                                                                *
      *                 d-prz-acq-cod-fnt = Codice fornitore           *
      *                                                                *
      *                 d-prz-acq-tip-mag = Tipo magazzino             *
      *                                                                *
      *                 d-prz-acq-num-mag = Codice numerico magazzino  *
      *                                                                *
      *                 d-prz-acq-fda-pif = Formato di acquisto        *
      *                                                                *
      *                 d-prz-acq-prz-lsb = Prezzo di listino base     *
      *                                                                *
      *                 d-prz-acq-dec-vlb = Decimali valuta listino    *
      *                                     base                       *
      *                                                                *
      *                 d-prz-acq-dec-prz = Decimali prezzo            *
      *                                                                *
      *                 d-prz-acq-qta-rif = Quantita' di riferimento   *
      *                                                                *
      *                 d-prz-acq-dat-rfp = Data riferimento prezzo    *
      *                                                                *
      *                                                                *
      *        Output : d-prz-acq-prz-acq = Prezzo di acquisto         *
      *                                                                *
      *                 d-prz-acq-psr-det = % di sconto                *
      *                                                                *
      *                 d-prz-acq-psr-snx = si/no % di sconto deter-   *
      *                                     minate                     *
      *                                                                *
      *                 d-prz-acq-dat-rfp = Data riferimento prezzo    *
      *                                     determinato                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "PN" - Determinazione prezzo netto, eventualmente trasformato  *
      *        all'unita' di misura per la vendita                     *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-acq-tip-ope = "PN"                       *
      *                                                                *
      *                 (come per il tipo operazione "PA")             *
      *                                                                *
      *        Output : d-prz-acq-prz-acq = Prezzo di acquisto al net- *
      *                                     to di sconti ed eventuale  *
      *                                     trasformazione unita' di   *
      *                                     misura                     *
      *                                                                *
      *                 d-prz-acq-dec-prz = Decimali prezzo della      *
      *                                     scheda fornitore           *
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
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [lfd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rflfd"                          .

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
      *        * Work per determinazione prezzo di acquisto            *
      *        *-------------------------------------------------------*
           05  w-det-prz-acq.
               10  w-det-prz-acq-inx      pic  9(02)                  .
               10  w-det-prz-acq-wpz      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione prezzi e sconti storicizzati  *
      *        *-------------------------------------------------------*
           05  w-det-prz-sto.
               10  w-det-prz-sto-flg      pic  x(01)                  .
               10  w-det-prz-sto-inx      pic  9(02)                  .
               10  w-det-prz-sto-wpz      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per data riferimento prezzo                      *
      *        *-------------------------------------------------------*
           05  w-det-dat-rif.
               10  w-det-dat-rif-dat      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per calcolo prezzo netto                        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione prezzo di vendi- *
      *    * ta                                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dprzacq0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-prz-acq              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-prz-acq-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-prz-acq-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-prz-acq-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-prz-acq-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di acquisto           *
      *                  *---------------------------------------------*
           else if   d-prz-acq-tip-ope    =    "PA"
                     perform dpa-000      thru dpa-999
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo netto trasformato     *
      *                  *---------------------------------------------*
           else if   d-prz-acq-tip-ope    =    "PN"
                     perform dpn-000      thru dpn-999                .
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
       opn-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * [lfd]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
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
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * [lfd]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
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
                     move  spaces         to   d-prz-acq-exi-sts
           else      move  "#"            to   d-prz-acq-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di acquisto                         *
      *    *-----------------------------------------------------------*
       dpa-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione data di riferimento prezzo      *
      *              *-------------------------------------------------*
           move      d-prz-acq-dat-rfp    to   w-det-dat-rif-dat      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-prz-acq-prz-acq      .
           move      zero                 to   d-prz-acq-psr-det (1)  .
           move      zero                 to   d-prz-acq-psr-det (2)  .
           move      zero                 to   d-prz-acq-psr-det (3)  .
           move      zero                 to   d-prz-acq-psr-det (4)  .
           move      zero                 to   d-prz-acq-psr-det (5)  .
           move      spaces               to   d-prz-acq-psr-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record [aaf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Lettura record [aaf]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM"             to   f-key                  .
           move      d-prz-acq-tip-mag    to   rf-aaf-tip-mag         .
           move      d-prz-acq-num-mag    to   rf-aaf-num-pro         .
           move      d-prz-acq-cod-fnt    to   rf-aaf-cod-dcf         .
           move      d-prz-acq-fda-pif    to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura record [aaf]          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpa-900.
       dpa-040.
      *              *-------------------------------------------------*
      *              * Lettura eventuali prezzi e sconti storicizzati  *
      *              *-------------------------------------------------*
           perform   dpa-sto-000          thru dpa-sto-999            .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-det-prz-sto-flg    not  = spaces
                     go to dpa-900.
       dpa-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo applicazione    *
      *              * prezzi e sconti                                 *
      *              *-------------------------------------------------*
           go to     dpa-100
                     dpa-200
                     depending            on   rf-aaf-tap-pes         .
       dpa-100.
      *              *-------------------------------------------------*
      *              * Tipo applicazione : indipendentemente dalla     *
      *              * quantita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo prezzo di   *
      *                  * acquisto                                    *
      *                  *---------------------------------------------*
           go to     dpa-110
                     dpa-120
                     depending            on   rf-aaf-tip-pza         .
       dpa-110.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : specifico per     *
      *                  * l'acquisto                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      rf-aaf-prz-pes (1)   to   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * Data aggiornamento                      *
      *                      *-----------------------------------------*
           if        rf-aaf-uda-pes       not  = zero
                     move  rf-aaf-uda-pes to   d-prz-acq-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-150.
       dpa-120.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : stesso prezzo del *
      *                  * listino base                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      d-prz-acq-prz-lsb    to   w-det-prz-acq-wpz      .
           if        d-prz-acq-dec-vlb    =    1
                     divide 10            into w-det-prz-acq-wpz
           else if   d-prz-acq-dec-vlb    =    2
                     divide 100           into w-det-prz-acq-wpz      .
           move      w-det-prz-acq-wpz    to   d-prz-acq-prz-acq      .
           if        d-prz-acq-dec-prz    =    1
                     multiply 10          by   d-prz-acq-prz-acq
           else if   d-prz-acq-dec-prz    =    2
                     multiply 100         by   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-150.
       dpa-150.
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           move      rf-aaf-psr-pes (1, 1)
                                          to   d-prz-acq-psr-det (1)  .
           move      rf-aaf-psr-pes (1, 2)
                                          to   d-prz-acq-psr-det (2)  .
           move      rf-aaf-psr-pes (1, 3)
                                          to   d-prz-acq-psr-det (3)  .
           move      rf-aaf-psr-pes (1, 4)
                                          to   d-prz-acq-psr-det (4)  .
           move      rf-aaf-psr-pes (1, 5)
                                          to   d-prz-acq-psr-det (5)  .
      *                  *---------------------------------------------*
      *                  * Segnale di sconti determinati               *
      *                  *---------------------------------------------*
           move      "#"                  to   d-prz-acq-psr-snx      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dpa-900.
       dpa-200.
      *              *-------------------------------------------------*
      *              * Tipo applicazione : a seconda della quantita'   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione dell'indice su tabella prez- *
      *                  * zi e sconti                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prz-acq-inx      .
       dpa-210.
           add       1                    to   w-det-prz-acq-inx      .
           if        w-det-prz-acq-inx    >    6
                     move  6              to   w-det-prz-acq-inx
                     go to dpa-220.
      *                      *-----------------------------------------*
      *                      * Test se elemento vuoto                  *
      *                      *-----------------------------------------*
           if        rf-aaf-qta-pes
                    (w-det-prz-acq-inx)   =    zero and
                     rf-aaf-prz-pes
                    (w-det-prz-acq-inx)   =    zero and
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 1)
                                          =    zero and
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 2)
                                          =    zero and
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 3)
                                          =    zero and
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 4)
                                          =    zero and
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 5)
                                          =    zero
                     go to dpa-220.
      *                      *-----------------------------------------*
      *                      * Test se elemento 'oltre'                *
      *                      *-----------------------------------------*
           if       (rf-aaf-qta-pes
                    (w-det-prz-acq-inx)   =    zero    ) and
                    (rf-aaf-prz-pes
                    (w-det-prz-acq-inx)   not  = zero or
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 1)
                                          not  = zero or
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 2)
                                          not  = zero or
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 3)
                                          not  = zero or
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 4)
                                          not  = zero or
                     rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 5)
                                          not  = zero  )
                     go to dpa-220.
      *                      *-----------------------------------------*
      *                      * Test su quantita'                       *
      *                      *-----------------------------------------*
           if        d-prz-acq-qta-rif    >    rf-aaf-qta-pes
                                              (w-det-prz-acq-inx)
                     go to dpa-210.
       dpa-220.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo prezzo di   *
      *                  * acquisto                                    *
      *                  *---------------------------------------------*
           go to     dpa-230
                     dpa-240
                     depending            on   rf-aaf-tip-pza         .
       dpa-230.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : specifico per     *
      *                  * l'acquisto                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      rf-aaf-prz-pes
                    (w-det-prz-acq-inx)   to   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * Data aggiornamento                      *
      *                      *-----------------------------------------*
           if        rf-aaf-uda-pes       not  = zero
                     move  rf-aaf-uda-pes to   d-prz-acq-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-250.
       dpa-240.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : stesso prezzo del *
      *                  * listino base                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      d-prz-acq-prz-lsb    to   w-det-prz-acq-wpz      .
           if        d-prz-acq-dec-vlb    =    1
                     divide 10            into w-det-prz-acq-wpz
           else if   d-prz-acq-dec-vlb    =    2
                     divide 100           into w-det-prz-acq-wpz      .
           move      w-det-prz-acq-wpz    to   d-prz-acq-prz-acq      .
           if        d-prz-acq-dec-prz    =    1
                     multiply 10          by   d-prz-acq-prz-acq
           else if   d-prz-acq-dec-prz    =    2
                     multiply 100         by   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-250.
       dpa-250.
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           move      rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 1)
                                          to   d-prz-acq-psr-det (1)  .
           move      rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 2)
                                          to   d-prz-acq-psr-det (2)  .
           move      rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 3)
                                          to   d-prz-acq-psr-det (3)  .
           move      rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 4)
                                          to   d-prz-acq-psr-det (4)  .
           move      rf-aaf-psr-pes
                    (w-det-prz-acq-inx, 5)
                                          to   d-prz-acq-psr-det (5)  .
      *                  *---------------------------------------------*
      *                  * Segnale di sconti determinati               *
      *                  *---------------------------------------------*
           move      "#"                  to   d-prz-acq-psr-snx      .
       dpa-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     dpa-999.
       dpa-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di acquisto                         *
      *    *                                                           *
      *    * Subroutine di determinazione prezzi e sconti storicizzati *
      *    *-----------------------------------------------------------*
       dpa-sto-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-prz-sto-flg      .
       dpa-sto-010.
      *              *-------------------------------------------------*
      *              * Start su file [lfd]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTFNTDVF "         to   f-key                  .
           move      01                   to   rf-lfd-tip-rec         .
           move      d-prz-acq-cod-fnt    to   rf-lfd-cod-dcf         .
           move      rf-aaf-sgl-vlt       to   rf-lfd-sgl-vlt         .
           move      d-prz-acq-tip-mag    to   rf-lfd-tip-mag         .
           move      d-prz-acq-num-mag    to   rf-lfd-num-mag         .
           move      d-prz-acq-fda-pif    to   rf-lfd-fda-pif         .
           move      w-det-dat-rif-dat    to   rf-lfd-dva-fin         .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpa-sto-900.
       dpa-sto-020.
      *              *-------------------------------------------------*
      *              * Lettura primo record [lfd]                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpa-sto-900.
       dpa-sto-030.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-lfd-tip-rec       not  = 01                or
                     rf-lfd-cod-dcf       not  = d-prz-acq-cod-fnt or
                     rf-lfd-sgl-vlt       not  = rf-aaf-sgl-vlt    or
                     rf-lfd-tip-mag       not  = d-prz-acq-tip-mag or
                     rf-lfd-num-mag       not  = d-prz-acq-num-mag or
                     rf-lfd-fda-pif       not  = d-prz-acq-fda-pif
                     go to dpa-sto-900.
       dpa-sto-040.
      *              *-------------------------------------------------*
      *              * Flag di determinazione effettuata : Si          *
      *              *-------------------------------------------------*
           move      "#"                  to   w-det-prz-sto-flg      .
       dpa-sto-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo applicazione    *
      *              * prezzi e sconti                                 *
      *              *-------------------------------------------------*
           go to     dpa-sto-100
                     dpa-sto-200
                     depending            on   rf-aaf-tap-pes         .
       dpa-sto-100.
      *              *-------------------------------------------------*
      *              * Tipo applicazione : indipendentemente dalla     *
      *              * quantita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo prezzo di   *
      *                  * acquisto                                    *
      *                  *---------------------------------------------*
           go to     dpa-sto-110
                     dpa-sto-120
                     depending            on   rf-aaf-tip-pza         .
       dpa-sto-110.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : specifico per     *
      *                  * l'acquisto                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      rf-lfd-prz-pes (1)   to   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * Data aggiornamento                      *
      *                      *-----------------------------------------*
           if        rf-lfd-dva-fin       not  = zero
                     move  rf-lfd-dva-fin to   d-prz-acq-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-sto-150.
       dpa-sto-120.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : stesso prezzo del *
      *                  * listino base                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      d-prz-acq-prz-lsb    to   w-det-prz-sto-wpz      .
           if        d-prz-acq-dec-vlb    =    1
                     divide 10            into w-det-prz-sto-wpz
           else if   d-prz-acq-dec-vlb    =    2
                     divide 100           into w-det-prz-sto-wpz      .
           move      w-det-prz-sto-wpz    to   d-prz-acq-prz-acq      .
           if        d-prz-acq-dec-prz    =    1
                     multiply 10          by   d-prz-acq-prz-acq
           else if   d-prz-acq-dec-prz    =    2
                     multiply 100         by   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-sto-150.
       dpa-sto-150.
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           move      rf-lfd-psr-pes (1, 1)
                                          to   d-prz-acq-psr-det (1)  .
           move      rf-lfd-psr-pes (1, 2)
                                          to   d-prz-acq-psr-det (2)  .
           move      rf-lfd-psr-pes (1, 3)
                                          to   d-prz-acq-psr-det (3)  .
           move      rf-lfd-psr-pes (1, 4)
                                          to   d-prz-acq-psr-det (4)  .
           move      rf-lfd-psr-pes (1, 5)
                                          to   d-prz-acq-psr-det (5)  .
      *                  *---------------------------------------------*
      *                  * Segnale di sconti determinati               *
      *                  *---------------------------------------------*
           move      "#"                  to   d-prz-acq-psr-snx      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dpa-sto-900.
       dpa-sto-200.
      *              *-------------------------------------------------*
      *              * Tipo applicazione : a seconda della quantita'   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione dell'indice su tabella prez- *
      *                  * zi e sconti                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prz-sto-inx      .
       dpa-sto-210.
           add       1                    to   w-det-prz-sto-inx      .
           if        w-det-prz-sto-inx    >    6
                     move  6              to   w-det-prz-sto-inx
                     go to dpa-sto-220.
      *                      *-----------------------------------------*
      *                      * Test se elemento vuoto                  *
      *                      *-----------------------------------------*
           if        rf-lfd-qta-pes
                    (w-det-prz-sto-inx)   =    zero and
                     rf-lfd-prz-pes
                    (w-det-prz-sto-inx)   =    zero and
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 1)
                                          =    zero and
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 2)
                                          =    zero and
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 3)
                                          =    zero and
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 4)
                                          =    zero and
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 5)
                                          =    zero
                     go to dpa-sto-220.
      *                      *-----------------------------------------*
      *                      * Test se elemento 'oltre'                *
      *                      *-----------------------------------------*
           if       (rf-lfd-qta-pes
                    (w-det-prz-sto-inx)   =    zero    ) and
                    (rf-lfd-prz-pes
                    (w-det-prz-sto-inx)   not  = zero or
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 1)
                                          not  = zero or
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 2)
                                          not  = zero or
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 3)
                                          not  = zero or
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 4)
                                          not  = zero or
                     rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 5)
                                          not  = zero  )
                     go to dpa-sto-220.
      *                      *-----------------------------------------*
      *                      * Test su quantita'                       *
      *                      *-----------------------------------------*
           if        d-prz-acq-qta-rif    >    rf-lfd-qta-pes
                                              (w-det-prz-sto-inx)
                     go to dpa-sto-210.
       dpa-sto-220.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo prezzo di   *
      *                  * acquisto                                    *
      *                  *---------------------------------------------*
           go to     dpa-sto-230
                     dpa-sto-240
                     depending            on   rf-aaf-tip-pza         .
       dpa-sto-230.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : specifico per     *
      *                  * l'acquisto                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      rf-lfd-prz-pes
                    (w-det-prz-sto-inx)   to   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * Data aggiornamento                      *
      *                      *-----------------------------------------*
           if        rf-lfd-dva-fin       not  = zero
                     move  rf-lfd-dva-fin to   d-prz-acq-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-sto-250.
       dpa-sto-240.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo di acquisto : stesso prezzo del *
      *                  * listino base                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
           move      d-prz-acq-prz-lsb    to   w-det-prz-sto-wpz      .
           if        d-prz-acq-dec-vlb    =    1
                     divide 10            into w-det-prz-sto-wpz
           else if   d-prz-acq-dec-vlb    =    2
                     divide 100           into w-det-prz-sto-wpz      .
           move      w-det-prz-sto-wpz    to   d-prz-acq-prz-acq      .
           if        d-prz-acq-dec-prz    =    1
                     multiply 10          by   d-prz-acq-prz-acq
           else if   d-prz-acq-dec-prz    =    2
                     multiply 100         by   d-prz-acq-prz-acq      .
      *                      *-----------------------------------------*
      *                      * A determinazione percentuali di sconto  *
      *                      *-----------------------------------------*
           go to     dpa-sto-250.
       dpa-sto-250.
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           move      rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 1)
                                          to   d-prz-acq-psr-det (1)  .
           move      rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 2)
                                          to   d-prz-acq-psr-det (2)  .
           move      rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 3)
                                          to   d-prz-acq-psr-det (3)  .
           move      rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 4)
                                          to   d-prz-acq-psr-det (4)  .
           move      rf-lfd-psr-pes
                    (w-det-prz-sto-inx, 5)
                                          to   d-prz-acq-psr-det (5)  .
      *                  *---------------------------------------------*
      *                  * Segnale di sconti determinati               *
      *                  *---------------------------------------------*
           move      "#"                  to   d-prz-acq-psr-snx      .
       dpa-sto-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpa-sto-999.
       dpa-sto-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di acquisto netto e trasformato     *
      *    *-----------------------------------------------------------*
       dpn-000.
      *              *-------------------------------------------------*
      *              * Determinazione prezzo lordo di acquisto         *
      *              *-------------------------------------------------*
           perform   dpa-000              thru dpa-999                .
      *              *-------------------------------------------------*
      *              * Bufferizzazione decimali prezzo fornitore       *
      *              *-------------------------------------------------*
           move      rf-aaf-dec-prz       to   d-prz-acq-dec-prz      .
       dpn-100.
      *              *-------------------------------------------------*
      *              * Determinazione prezzo netto                     *
      *              *-------------------------------------------------*
           move      d-prz-acq-prz-acq    to   w-cal-prz-net-prz      .
           move      d-prz-acq-psr-det (1)
                                          to   w-cal-prz-net-psc (1)  .
           move      d-prz-acq-psr-det (2)
                                          to   w-cal-prz-net-psc (2)  .
           move      d-prz-acq-psr-det (3)
                                          to   w-cal-prz-net-psc (3)  .
           move      d-prz-acq-psr-det (4)
                                          to   w-cal-prz-net-psc (4)  .
           move      d-prz-acq-psr-det (5)
                                          to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .

           move      w-cal-prz-net-prz    to   d-prz-acq-prz-acq      .
       dpn-200.
      *              *-------------------------------------------------*
      *              * Eventuale trasformazione unita' di misura       *
      *              *-------------------------------------------------*
           if        d-prz-acq-prz-acq    =    zero
                     go to dpn-900.
           if        rf-aaf-snx-tum       not  = "S" and
                     rf-aaf-snx-tum       not  = "P"
                     go to dpn-900.
       dpn-300.
      *              *-------------------------------------------------*
      *              * Trasformazione unita' di misura                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-aaf-snx-tum       not  = "S"
                     go to dpn-500.
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           multiply  rf-aaf-cdi-tum       by   d-prz-acq-prz-acq      .
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           divide    rf-aaf-cmo-tum       into d-prz-acq-prz-acq      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dpn-900.
       dpn-500.
      *              *-------------------------------------------------*
      *              * Trasformazione unita' di misura solo per prezzo *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore determinato e' a zero: ad      *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        d-prz-acq-prz-acq    =    zero
                     go to dpn-900.
      *                  *---------------------------------------------*
      *                  * Test se possibile aumentare i decimali      *
      *                  *---------------------------------------------*
           if        d-prz-acq-dec-prz    >    zero
                     go to dpn-520.
      *                  *---------------------------------------------*
      *                  * Si ritara il numero decimali, ove possibile *
      *                  * in relazione al coefficiente divisore, per  *
      *                  * ottenere un costo unitario corretto         *
      *                  *                                             *
      *                  * Si presume che siano coefficienti relativi  *
      *                  * all'acquisto in decine o centinaia          *
      *                  *---------------------------------------------*
           if        rf-aaf-cmo-tum       =    10
                     multiply  10         by   d-prz-acq-prz-acq
                     add       1          to   d-prz-acq-dec-prz
           else if   rf-aaf-cmo-tum       =    100
                     multiply  100        by   d-prz-acq-prz-acq
                     add       2          to   d-prz-acq-dec-prz      .
       dpn-520.
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           multiply  rf-aaf-cdi-tum       by   d-prz-acq-prz-acq      .
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           divide    rf-aaf-cmo-tum       into d-prz-acq-prz-acq      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dpn-900.
       dpn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpn-999.
       dpn-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

