       Identification Division.
       Program-Id.                                 dsstssp0           .
      *================================================================*
      *                                                                *
      * Modulo per la determinazione relativamente ad un prodotto di : *
      *                                                                *
      * - Dati statistici di acquisto e vendita                        *
      *                                                                *
      *        ____ MODULO NON UTILIZZATO ____                         *
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
      *        Input  : d-sst-ssp-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-sst-ssp-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sst-ssp-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-sst-ssp-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione dati statistici, in valuta base          *
      *                                                                *
      *                                                                *
      *        Input  : d-sst-ssp-tip-ope = "DT"                       *
      *                                                                *
      *                 d-sst-ssp-cod-dpz = Codice dipendenza          *
      *                                    (se zero = tutte)           *
      *                                                                *
      *                 d-sst-ssp-num-pro = Codice numerico prodotto   *
      *                                                                *
      *                 d-sst-ssp-dat-rif = Data di riferimento        *
      *                                    (facoltativa)               *
      *                                                                *
      *                                                                *
      *        Output : d-sst-ssp-prg-mes = Progressivi mensili        *
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
      *        * [ssp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/sst/fls/rec/rfssp"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  filler                     pic  x(01)                  .

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
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det progressivi mensili                      *
      *        *-------------------------------------------------------*
           05  w-det-prg-mes.
      *            *---------------------------------------------------*
      *            * Anno di esercizio                                 *
      *            *---------------------------------------------------*
               10  w-det-prg-mes-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese di esercizio                                 *
      *            *---------------------------------------------------*
               10  w-det-prg-mes-mes      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-det-prg-mes-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-prg-mes-ctr      pic  9(02)                  .
               10  w-det-prg-mes-ctd      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione dati statistici  *
      *    * prodotto                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/sst/prg/cpy/dsstssp0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-sst-ssp              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-sst-ssp-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-sst-ssp-tip-ope    =    "DT"
                     perform det-sst-ssp-000
                                          thru det-sst-ssp-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-sst-ssp-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-sst-ssp-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-sst-ssp-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione data di riferimento             *
      *              *-------------------------------------------------*
           move      zero                 to   d-sst-ssp-dat-rif      .
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
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ssp]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/sst/fls/ioc/obj/iofssp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ssp                 .
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
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ssp]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/sst/fls/ioc/obj/iofssp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ssp                 .
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
                     move  spaces         to   d-sst-ssp-exi-sts
           else      move  "#"            to   d-sst-ssp-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione fatturato annuo cliente, in valuta base    *
      *    *-----------------------------------------------------------*
       det-sst-ssp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare codice dipendenza   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-prg-mes-dpz      .
      *              *-------------------------------------------------*
      *              * Test se codici dipendenza in input              *
      *              *-------------------------------------------------*
           if        d-sst-ssp-cod-dpz    not  = zero
                     go to det-sst-ssp-050.
      *              *-------------------------------------------------*
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      spaces               to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       det-sst-ssp-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali esercizio                            *
      *                  *---------------------------------------------*
           move      zero                 to   d-sst-ssp-tot-qta      .
           move      zero                 to   d-sst-ssp-tot-vaa      .
           move      zero                 to   d-sst-ssp-tot-qtv      .
           move      zero                 to   d-sst-ssp-tot-vav      .
      *                  *---------------------------------------------*
      *                  * Progressivi mensili                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prg-mes-ctr      .
       det-sst-ssp-100.
           add       1                    to   w-det-prg-mes-ctr      .
           if        w-det-prg-mes-ctr    >    12
                     go to det-sst-ssp-200.
      *
           move      zero                 to   d-sst-ssp-qta-acq
                                              (w-det-prg-mes-ctr)     .
           move      zero                 to   d-sst-ssp-val-acq
                                              (w-det-prg-mes-ctr)     .
           move      zero                 to   d-sst-ssp-qta-ven
                                              (w-det-prg-mes-ctr)     .
           move      zero                 to   d-sst-ssp-val-ven
                                              (w-det-prg-mes-ctr)     .
      *
           go to     det-sst-ssp-100.
       det-sst-ssp-200.
      *              *-------------------------------------------------*
      *              * Preparazione anno di esercizio                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione se esiste o meno la data di ri-  *
      *                  * ferimento                                   *
      *                  *---------------------------------------------*
           if        d-sst-ssp-dat-rif    =    zero
                     go to det-sst-ssp-240.
       det-sst-ssp-220.
      *                  *---------------------------------------------*
      *                  * Se esiste la data di riferimento            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scomposizione data                      *
      *                      *-----------------------------------------*
           move      d-sst-ssp-dat-rif    to   s-dat                  .
           move      s-saa                to   w-det-prg-mes-ese      .
           move      s-mes                to   w-det-prg-mes-mes      .
      *                      *-----------------------------------------*
      *                      * A determinazione                        *
      *                      *-----------------------------------------*
           go to     det-sst-ssp-300.
       det-sst-ssp-240.
      *                  *---------------------------------------------*
      *                  * Se non esiste la data di riferimento        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Scomposizione data                      *
      *                      *-----------------------------------------*
           move      s-saa                to   w-det-prg-mes-ese      .
           move      s-mes                to   w-det-prg-mes-mes      .
      *                      *-----------------------------------------*
      *                      * A determinazione                        *
      *                      *-----------------------------------------*
           go to     det-sst-ssp-300.
       det-sst-ssp-300.
      *              *-------------------------------------------------*
      *              * Test se scansione dipendenze necessario         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        d-sst-ssp-cod-dpz    =    zero
                     go to det-sst-ssp-400.
      *                  *---------------------------------------------*
      *                  * Comodo per scansione                        *
      *                  *---------------------------------------------*
           move      d-sst-ssp-cod-dpz    to   w-det-prg-mes-dpz      .
      *                  *---------------------------------------------*
      *                  * Determinazione per singola dipendenza       *
      *                  *---------------------------------------------*
           perform   det-sst-ssp-dpz-000  thru det-sst-ssp-dpz-999    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sst-ssp-900.
       det-sst-ssp-400.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione dipendenze                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio ciclo                                *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prg-mes-ctd      .
       det-sst-ssp-420.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-prg-mes-ctd      .
      *                  *---------------------------------------------*
      *                  * Test se raggiunto massimo numero dipendenze *
      *                  *---------------------------------------------*
           if        w-det-prg-mes-ctd    >    w-dpz-ctr-dpz
                     go to det-sst-ssp-900.
      *                  *---------------------------------------------*
      *                  * Determinazione per singola dipendenza       *
      *                  *---------------------------------------------*
           if        w-dpz-ele-flg
                    (w-det-prg-mes-ctd)   not  = spaces
                     move  w-det-prg-mes-ctd
                                          to   w-det-prg-mes-dpz
           else      go to det-sst-ssp-420.
      *                  *---------------------------------------------*
      *                  * Determinazione per singola dipendenza       *
      *                  *---------------------------------------------*
           perform   det-sst-ssp-dpz-000  thru det-sst-ssp-dpz-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-sst-ssp-420.
       det-sst-ssp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sst-ssp-999.
       det-sst-ssp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione fatturato annuo cliente, in valuta base    *
      *    *                                                           *
      *    * Subroutine per dipendenza                                 *
      *    *-----------------------------------------------------------*
       det-sst-ssp-dpz-000.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/sst/fls/ioc/obj/iofssp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ssp                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DPZPRO    "         to   f-key                  .
           move      w-det-prg-mes-ese    to   rf-ssp-ann-ese         .
           move      w-det-prg-mes-dpz    to   rf-ssp-cod-dpz         .
           move      d-sst-ssp-num-pro    to   rf-ssp-num-pro         .
           move      d-sst-ssp-var-mag    to   rf-ssp-var-mag         .
           move      "pgm/sst/fls/ioc/obj/iofssp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ssp                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sst-ssp-dpz-900.
       det-sst-ssp-dpz-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori letti                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-prg-mes-ctr      .
       det-sst-ssp-dpz-420.
           add       1                    to   w-det-prg-mes-ctr      .
           if        w-det-prg-mes-ctr    >    12
                     go to det-sst-ssp-dpz-900.
      *                  *---------------------------------------------*
      *                  * Incremento progressivi mensili              *
      *                  *---------------------------------------------*
           add       rf-ssp-qta-acq
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-qta-acq
                                              (w-det-prg-mes-ctr)     .
      *
           add       rf-ssp-val-acq
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-val-acq
                                              (w-det-prg-mes-ctr)     .
      *
           add       rf-ssp-qta-ven
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-qta-ven
                                              (w-det-prg-mes-ctr)     .
      *
           add       rf-ssp-val-ven
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-val-ven
                                              (w-det-prg-mes-ctr)     .
      *                  *---------------------------------------------*
      *                  * Incremento totali                           *
      *                  *---------------------------------------------*
           add       rf-ssp-qta-acq
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-tot-qta     .
           add       rf-ssp-val-acq
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-tot-vaa     .
           add       rf-ssp-qta-ven
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-tot-qtv     .
           add       rf-ssp-val-ven
                    (w-det-prg-mes-ctr)   to   d-sst-ssp-tot-vav     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-sst-ssp-dpz-420.
       det-sst-ssp-dpz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sst-ssp-dpz-999.
       det-sst-ssp-dpz-999.
           exit.


