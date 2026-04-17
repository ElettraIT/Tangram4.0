       Identification Division.
       Program-Id.                                 dfathfc0           .
      *================================================================*
      *                                                                *
      * Modulo per la determinazione, relativamente ad un cliente di : *
      *                                                                *
      * - Fatturato annuo, in valuta base                              *
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
      *        Input  : d-fat-hfc-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-fat-hfc-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-fat-hfc-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-fat-hfc-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione fatturato annuo, in valuta base          *
      *                                                                *
      *                                                                *
      *        Input  : d-fat-hfc-tip-ope = "DT"                       *
      *                                                                *
      *                 d-fat-hfc-cod-cli = Codice cliente             *
      *                                                                *
      *                 d-fat-hfc-dat-rif = Data di riferimento        *
      *                                     (facoltativa)              *
      *                                                                *
      *                                                                *
      *        Output : d-fat-hfc-fat-ann = Fatturato annuo, in valuta *
      *                                     base                       *
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
      *        * [hfc]                                                 *
      *        *-------------------------------------------------------*
           copy      "sir/agb/fls/rec/rfhfc"                          .

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
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det fatturato annuo, in valuta base          *
      *        *-------------------------------------------------------*
           05  w-det-fat-ann.
      *            *---------------------------------------------------*
      *            * Anno di esercizio                                 *
      *            *---------------------------------------------------*
               10  w-det-fat-ann-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese di esercizio                                 *
      *            *---------------------------------------------------*
               10  w-det-fat-ann-mes      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-fat-ann-ctr      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione fatturato annuo  *
      *    * cliente, in valuta base                                   *
      *    *-----------------------------------------------------------*
           copy      "sir/agb/prg/cpy/dfathfc0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-fat-hfc              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-fat-hfc-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-fat-hfc-tip-ope    =    "DT"
                     perform det-fat-hfc-000
                                          thru det-fat-hfc-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-fat-hfc-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-fat-hfc-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-fat-hfc-tip-ope    =    "C?"
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
           move      zero                 to   d-fat-hfc-dat-rif      .
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
      *                  * [hfc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhfc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
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
      *                  * [hfc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhfc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
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
                     move  spaces         to   d-fat-hfc-exi-sts
           else      move  "#"            to   d-fat-hfc-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione fatturato annuo cliente, in valuta base    *
      *    *-----------------------------------------------------------*
       det-fat-hfc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-fat-hfc-fat-ann      .
      *              *-------------------------------------------------*
      *              * Preparazione anno di esercizio                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione se esiste o meno la data di ri-  *
      *                  * ferimento                                   *
      *                  *---------------------------------------------*
           if        d-fat-hfc-dat-rif    =    zero
                     go to det-fat-hfc-040.
       det-fat-hfc-020.
      *                  *---------------------------------------------*
      *                  * Se esiste la data di riferimento            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scomposizione data                      *
      *                      *-----------------------------------------*
           move      d-fat-hfc-dat-rif    to   s-dat                  .
           move      s-saa                to   w-det-fat-ann-ese      .
           move      s-mes                to   w-det-fat-ann-mes      .
      *                      *-----------------------------------------*
      *                      * A determinazione                        *
      *                      *-----------------------------------------*
           go to     det-fat-hfc-100.
       det-fat-hfc-040.
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
           move      s-saa                to   w-det-fat-ann-ese      .
           move      s-mes                to   w-det-fat-ann-mes      .
      *                      *-----------------------------------------*
      *                      * A determinazione                        *
      *                      *-----------------------------------------*
           go to     det-fat-hfc-100.
       det-fat-hfc-100.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su record [hfc] per cliente           *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ESECLIAGE "         to   f-key                  .
           move      w-det-fat-ann-ese    to   rf-hfc-ann-ese         .
           move      d-fat-hfc-cod-cli    to   rf-hfc-cod-cli         .
           move      zero                 to   rf-hfc-cod-age         .
           move      "sir/agb/fls/ioc/obj/iofhfc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : fine ciclo                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-fat-hfc-900.
       det-fat-hfc-200.
      *                  *---------------------------------------------*
      *                  * Read Next da [hfc]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhfc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
      *                      *-----------------------------------------*
      *                      * Se fine file : fine ciclo               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-fat-hfc-900.
       det-fat-hfc-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-hfc-ann-ese       not  = w-det-fat-ann-ese
                     go to det-fat-hfc-900.
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-hfc-cod-cli       not  = d-fat-hfc-cod-cli
                     go to det-fat-hfc-900.
       det-fat-hfc-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-fat-hfc-600.
      *              *-------------------------------------------------*
      *              * Ciclo di cumulo dei progressivi mensili         *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-fat-ann-ctr      .
       det-fat-hfc-620.
           add       1                    to   w-det-fat-ann-ctr      .
           if        w-det-fat-ann-ctr    >    12
                     go to det-fat-hfc-800.
           if        w-det-fat-ann-ctr    >    w-det-fat-ann-mes
                     go to det-fat-hfc-800.
           add       rf-hfc-fat-mns
                    (w-det-fat-ann-ctr)   to   d-fat-hfc-fat-ann      .
           go to     det-fat-hfc-620.
       det-fat-hfc-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-fat-hfc-200.
       det-fat-hfc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-fat-hfc-999.
       det-fat-hfc-999.
           exit.

