       Identification Division.
       Program-Id.                                 dimpacq0           .
      *================================================================*
      *                                                                *
      * Modulo di determinazione dell'importo in riga per l'acquisto   *
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
      *        Input  : d-imp-acq-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-acq-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-acq-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-imp-acq-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "IR" - Determinazione importo in riga                          *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-acq-tip-ope = "IR"                       *
      *                                                                *
      *                 d-imp-acq-qta-acq = Quantita' per il calcolo   *
      *                                                                *
      *                 d-imp-acq-prz-uni = Prezzo unitario            *
      *                                                                *
      *                 d-imp-acq-dec-prz = Decimali prezzo unitario   *
      *                                                                *
      *                 d-imp-acq-per-scr = % di sconto in riga        *
      *                                                                *
      *                 d-imp-acq-tip-clc = Tipo di calcolo            *
      *                                                                *
      *                                                                *
      *        Output : d-imp-acq-imp-rig = Importo determinato        *
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
      *    * Work-area per determinazione prezzo netto                 *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

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
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-det-ctr-001              pic  9(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dimpacq0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-imp-acq              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-imp-acq-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-imp-acq-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-imp-acq-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-imp-acq-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *---------------------------------------------*
           else if   d-imp-acq-tip-ope    =    "IR"
                     perform dir-000      thru dir-999                .
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
                     move  spaces         to   d-imp-acq-exi-sts
           else      move  "#"            to   d-imp-acq-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       dir-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-imp-acq-imp-rig      .
       dir-050.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di calcolo        *
      *              *-------------------------------------------------*
           if        d-imp-acq-tip-clc    =    spaces or
                     d-imp-acq-tip-clc    =    "M"
                     go to dir-100
           else if   d-imp-acq-tip-clc    =    "L"
                     go to dir-200
           else      go to dir-100.
       dir-100.
      *              *-------------------------------------------------*
      *              * Metodo standard : Prezzo netto x quantita'      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se prezzo a zero : ad uscita                *
      *                  *---------------------------------------------*
           if        d-imp-acq-prz-uni    =    zero
                     go to dir-900.
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo netto                 *
      *                  *---------------------------------------------*
           move      d-imp-acq-prz-uni    to   w-cal-prz-net-prz      .
           move      d-imp-acq-per-scr (1)
                                          to   w-cal-prz-net-psc (1)  .
           move      d-imp-acq-per-scr (2)
                                          to   w-cal-prz-net-psc (2)  .
           move      d-imp-acq-per-scr (3)
                                          to   w-cal-prz-net-psc (3)  .
           move      d-imp-acq-per-scr (4)
                                          to   w-cal-prz-net-psc (4)  .
           move      d-imp-acq-per-scr (5)
                                          to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  d-imp-acq-qta-acq    by   w-cal-prz-net-prz
                                        giving d-imp-acq-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ritaratura in base alla presenza di deci-   *
      *                  * mali per il prezzo                          *
      *                  *---------------------------------------------*
           if        d-imp-acq-dec-prz    =    1
                     divide 10            into d-imp-acq-imp-rig
           else if   d-imp-acq-dec-prz    =    2
                     divide 100           into d-imp-acq-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dir-900.
       dir-200.
      *              *-------------------------------------------------*
      *              * Metodo 'L' : Prezzo lordo x quantita', scontato *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se prezzo a zero : ad uscita                *
      *                  *---------------------------------------------*
           if        d-imp-acq-prz-uni    =    zero
                     go to dir-900.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  d-imp-acq-qta-acq    by   d-imp-acq-prz-uni
                                        giving d-imp-acq-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ciclo per abbattimento con sconti           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-ctr-001          .
       dir-220.
           add       1                    to   w-det-ctr-001          .
           if        w-det-ctr-001        >    5
                     go to dir-280.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di calcolo                 *
      *                  *---------------------------------------------*
           move      d-imp-acq-imp-rig    to   w-cal-imp-sco-iml      .
           move      d-imp-acq-per-scr
                    (w-det-ctr-001)       to   w-cal-imp-sco-psc      .
           perform   cal-imp-sco-000      thru cal-imp-sco-999        .
           move      w-cal-imp-sco-imn    to   d-imp-acq-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     dir-220.
       dir-280.
      *                  *---------------------------------------------*
      *                  * Ritaratura in base alla presenza di deci-   *
      *                  * mali per il prezzo                          *
      *                  *---------------------------------------------*
           if        d-imp-acq-dec-prz    =    1
                     divide 10            into d-imp-acq-imp-rig
           else if   d-imp-acq-dec-prz    =    2
                     divide 100           into d-imp-acq-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dir-900.
       dir-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dir-999.
       dir-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

