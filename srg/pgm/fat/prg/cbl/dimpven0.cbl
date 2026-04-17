       Identification Division.
       Program-Id.                                 dimpven0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:                        *
      *                                   Fase:    dimpven0            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/10/00    *
      *                       Ultima revisione:    NdK del 27/01/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo di determinazione dell'importo in riga per la vendita   *
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
      *        Input  : d-imp-ven-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-ven-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-ven-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-imp-ven-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "IR" - Determinazione importo in riga                          *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-ven-tip-ope = "IR"                       *
      *                                                                *
      *                 d-imp-ven-qta-ven = Quantita' primaria         *
      *                                                                *
      *                 d-imp-ven-snx-2qt = Si/no utilizzo 2. quantita'*
      *                                                                *
      *                 d-imp-ven-qta-a02 = 2. quantita'               *
      *                                                                *
      *                 d-imp-ven-snx-3qt = Si/no utilizzo 3. quantita'*
      *                                                                *
      *                 d-imp-ven-qta-a03 = 3. quantita'               *
      *                                                                *
      *                 d-imp-ven-prz-uni = Prezzo unitario            *
      *                                                                *
      *                 d-imp-ven-dec-prz = Decimali prezzo unitario   *
      *                                                                *
      *                                                                *
      *        Output : d-imp-ven-imp-rig = Importo determinato        *
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

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Tipo determinazione importo in riga per la vendita    *
      *        *-------------------------------------------------------*
           05  w-prs-tdi-ven              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo determinazione importo in riga per la vendita    *
      *        * in funzione di sconti e quantita'                     *
      *        *-------------------------------------------------------*
           05  w-prs-tdi-aps              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per determinazione importo in riga              *
      *    *-----------------------------------------------------------*
       01  w-dpv.
      *        *-------------------------------------------------------*
      *        * Comodo per il calcolo                                 *
      *        *-------------------------------------------------------*
           05  w-dpv-wrk-qta              pic s9(10)v9(03)            .

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
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-imp-ven              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-imp-ven-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-imp-ven-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-imp-ven-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-imp-ven-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *---------------------------------------------*
           else if   d-imp-ven-tip-ope    =    "IR"
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
      *                  *---------------------------------------------*
      *                  * Tipo determinazione importo in riga         *
      *                  *---------------------------------------------*
           perform   prs-tdi-ven-000      thru prs-tdi-ven-999        .
      *                  *---------------------------------------------*
      *                  * Tipo determinazione importo in riga in      *
      *                  * funzione di sconti e quantita'              *
      *                  *---------------------------------------------*
           perform   prs-tdi-aps-000      thru prs-tdi-aps-999        .
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
                     move  spaces         to   d-imp-ven-exi-sts
           else      move  "#"            to   d-imp-ven-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo determinazione importo   *
      *    *                             in riga per la vendita        *
      *    *-----------------------------------------------------------*
       prs-tdi-ven-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tdi-ven]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione lettura                         *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     move  zero           to   w-prs-tdi-ven
           else      move  s-num          to   w-prs-tdi-ven          .
      *              *-------------------------------------------------*
      *              * Regolarizzazione lettura                        *
      *              *-------------------------------------------------*
           if        w-prs-tdi-ven        not  = 02
                     move  01             to   w-prs-tdi-ven          .
       prs-tdi-ven-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo determinazione importo   *
      *    *                             in riga per la vendita in     *
      *    *                             funzione di sconti e quantita'*
      *    *-----------------------------------------------------------*
       prs-tdi-aps-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tdi-aps]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione lettura                         *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     move  zero           to   w-prs-tdi-aps
           else      move  s-num          to   w-prs-tdi-aps          .
      *              *-------------------------------------------------*
      *              * Regolarizzazione lettura                        *
      *              *-------------------------------------------------*
           if        w-prs-tdi-aps        not  = 02
                     move  01             to   w-prs-tdi-aps          .
       prs-tdi-aps-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       dir-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di calcolo        *
      *              *-------------------------------------------------*
           if        w-prs-tdi-aps        =    01
                     go to dir-100
           else if   w-prs-tdi-aps        =    02
                     go to dir-200
           else      go to dir-100.
       dir-100.
      *              *-------------------------------------------------*
      *              * Metodo '01' : Prezzo netto x quantita'          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   dir-std-000          thru dir-std-999            .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dir-900.
       dir-200.
      *              *-------------------------------------------------*
      *              * Metodo '02' : Prezzo lordo x qta', scontato     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   dir-lrs-000          thru dir-lrs-999            .
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
      *    * Determinazione importo in riga                            *
      *    *                                                           *
      *    * Subroutine Prezzo netto x quantita'                       *
      *    *-----------------------------------------------------------*
       dir-std-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-imp-ven-imp-rig      .
       dir-std-010.
      *              *-------------------------------------------------*
      *              * Preparazione preliminare della quantita'        *
      *              *-------------------------------------------------*
           if        d-imp-ven-snx-2qt    =    0
                     move  d-imp-ven-qta-ven
                                          to   w-dpv-wrk-qta
           else      move  d-imp-ven-qta-a02
                                          to   w-dpv-wrk-qta          .
       dir-std-050.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo determinazione    *
      *              * letto da personalizzazione                      *
      *              *-------------------------------------------------*
           if        w-prs-tdi-ven        =    01
                     go to dir-std-100
           else if   w-prs-tdi-ven        =    02
                     go to dir-std-200
           else      go to dir-std-100.
       dir-std-100.
      *              *-------------------------------------------------*
      *              * Metodo '01' : troncamento                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se prezzo a zero : ad uscita                *
      *                  *---------------------------------------------*
           if        d-imp-ven-prz-uni    =    zero
                     go to dir-std-900.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-dpv-wrk-qta        by   d-imp-ven-prz-uni
                                        giving d-imp-ven-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ritaratura in base alla presenza di deci-   *
      *                  * mali per il prezzo                          *
      *                  *                                             *
      *                  * N.B.: massimo 3 decimali prezzo             *
      *                  *---------------------------------------------*
           if        d-imp-ven-dec-prz    =    1
                     divide 10            into d-imp-ven-imp-rig
           else if   d-imp-ven-dec-prz    =    2
                     divide 100           into d-imp-ven-imp-rig
           else if   d-imp-ven-dec-prz    =    3
                     divide 1000          into d-imp-ven-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dir-std-900.
       dir-std-200.
      *              *-------------------------------------------------*
      *              * Metodo '02' : arrotondamento al 2. decimale     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in presenza dei decimali prezzo  *
      *                  *---------------------------------------------*
           if        d-imp-ven-dec-prz    =    0
                     go to dir-std-205
           else if   d-imp-ven-dec-prz    =    1
                     go to dir-std-210
           else if   d-imp-ven-dec-prz    =    2
                     go to dir-std-220
           else if   d-imp-ven-dec-prz    =    3
                     go to dir-std-230
           else      go to dir-std-205.
       dir-std-205.
      *                  *---------------------------------------------*
      *                  * Se '0' decimali prezzo                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           multiply  w-dpv-wrk-qta        by   d-imp-ven-prz-uni
                                        giving d-imp-ven-imp-rig
                                       rounded                        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dir-std-900.
       dir-std-210.
      *                  *---------------------------------------------*
      *                  * Se '1' decimale prezzo                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           multiply  w-dpv-wrk-qta        by   d-imp-ven-prz-uni
                                        giving d-imp-ven-imp-rig      .
      *                      *-----------------------------------------*
      *                      * Ritaratura in base alla presenza del    *
      *                      * decimale                                *
      *                      *-----------------------------------------*
           divide    10                   into d-imp-ven-imp-rig
                                       rounded                        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dir-std-900.
       dir-std-220.
      *                  *---------------------------------------------*
      *                  * Se '2' decimali prezzo                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           multiply  w-dpv-wrk-qta        by   d-imp-ven-prz-uni
                                        giving d-imp-ven-imp-rig      .
      *                      *-----------------------------------------*
      *                      * Ritaratura in base alla presenza del    *
      *                      * decimale                                *
      *                      *-----------------------------------------*
           divide    100                  into d-imp-ven-imp-rig
                                       rounded                        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dir-std-900.
       dir-std-230.
      *                  *---------------------------------------------*
      *                  * Se '3' decimali prezzo                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           multiply  w-dpv-wrk-qta        by   d-imp-ven-prz-uni
                                        giving d-imp-ven-imp-rig      .
      *                      *-----------------------------------------*
      *                      * Ritaratura in base alla presenza del    *
      *                      * decimale                                *
      *                      *-----------------------------------------*
           divide    1000                 into d-imp-ven-imp-rig
                                       rounded                        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dir-std-900.
       dir-std-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dir-std-999.
       dir-std-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *                                                           *
      *    * Subroutine Prezzo lordo x quantita', scontato             *
      *    *-----------------------------------------------------------*
       dir-lrs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-imp-ven-imp-rig      .
       dir-lrs-010.
      *              *-------------------------------------------------*
      *              * Preparazione preliminare della quantita'        *
      *              *-------------------------------------------------*
           if        d-imp-ven-snx-2qt    =    0
                     move  d-imp-ven-qta-ven
                                          to   w-dpv-wrk-qta
           else      move  d-imp-ven-qta-a02
                                          to   w-dpv-wrk-qta          .
       dir-lrs-100.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se prezzo a zero : ad uscita                *
      *                  *---------------------------------------------*
           if        d-imp-ven-prz-unl    =    zero
                     go to dir-lrs-900.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-dpv-wrk-qta        by   d-imp-ven-prz-unl
                                        giving d-imp-ven-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ciclo per abbattimento con sconti           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-ctr-001          .
       dir-lrs-220.
           add       1                    to   w-det-ctr-001          .
           if        w-det-ctr-001        >    5
                     go to dir-lrs-280.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di calcolo                 *
      *                  *---------------------------------------------*
           move      d-imp-ven-imp-rig    to   w-cal-imp-sco-iml      .
           move      d-imp-ven-per-scr
                    (w-det-ctr-001)       to   w-cal-imp-sco-psc      .
           perform   cal-imp-sco-000      thru cal-imp-sco-999        .
           move      w-cal-imp-sco-imn    to   d-imp-ven-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     dir-lrs-220.
       dir-lrs-280.
      *                  *---------------------------------------------*
      *                  * Ritaratura in base alla presenza di deci-   *
      *                  * mali per il prezzo                          *
      *                  *---------------------------------------------*
           if        d-imp-ven-dec-prz    =    1
                     divide 10            into d-imp-ven-imp-rig
           else if   d-imp-ven-dec-prz    =    2
                     divide 100           into d-imp-ven-imp-rig
           else if   d-imp-ven-dec-prz    =    3
                     divide 1000          into d-imp-ven-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dir-lrs-900.
       dir-lrs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dir-lrs-999.
       dir-lrs-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

