       Identification Division.
       Program-Id.                                 porc500o           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    sir                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    ric                 *
      *                                   Fase:    orc500              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/05/95    *
      *                       Ultima revisione:    NdK del 16/03/98    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Supporto per la ricezione ordini clienti    *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Situazione ordini fornitore                 *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Richiama il programma porf3010 dopo aver    *
      *                    preparato due opportune variabili di i.p.c. *
      *                    per non ammettere il Tasto Slct, e per for- *
      *                    zare il tipo di interrogazione              *
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
      *    * Work-area per il pathname del programma da richiamare     *
      *    *-----------------------------------------------------------*
       01  w-pat-nam.
      *        *-------------------------------------------------------*
      *        * Pathname originale                                    *
      *        *-------------------------------------------------------*
           05  w-pat-nam-ori              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Pathname filtrato                                     *
      *        *-------------------------------------------------------*
           05  w-pat-nam-flt              pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area referenze per determinazione accessi utente     *
      *    *-----------------------------------------------------------*
           copy      "sir/orc/prg/cpy/drefute0.dtl"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Referenze relative all'area 'sir'               *
      *              *-------------------------------------------------*
           perform   ref-sir-ute-000      thru ref-sir-ute-999        .
      *              *-------------------------------------------------*
      *              * Determinazione accesso utente                   *
      *              *-------------------------------------------------*
           perform   det-acc-ute-000      thru det-acc-ute-999        .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname originale del pro-    *
      *              * gramma da richiamare                            *
      *              *-------------------------------------------------*
           move      "pgm/orf/prg/obj/porf3010                "
                                          to   w-pat-nam-ori          .
      *              *-------------------------------------------------*
      *              * Filtro del pathname originale del programma da  *
      *              * richiamare                                      *
      *              *-------------------------------------------------*
           move      w-pat-nam-ori        to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   w-pat-nam-flt          .
       main-200.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. per il livello    *
      *              * successivo, per l'ammissibilita' del tasto      *
      *              * Slct                                            *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "N"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. per il livello    *
      *              * successivo, per l'ammissibilita' del tasto      *
      *              * Expd                                            *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-exp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           if        w-ref-sir-ute-fll    =    spaces
                     move  "S"            to   s-alf
           else      move  "N"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. per il livello    *
      *              * successivo, per il tipo di interrogazione da    *
      *              * eseguire                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      "INTDIF    "         to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       main-400.
      *              *-------------------------------------------------*
      *              * Richiamo programma                              *
      *              *-------------------------------------------------*
           call      w-pat-nam-flt                                    .
      *              *-------------------------------------------------*
      *              * Cancellazione programma                         *
      *              *-------------------------------------------------*
           cancel    w-pat-nam-flt                                    .
       main-999.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           exit      program                                          .

      *    *===========================================================*
      *    * Lettura delle referenze relative all'area 'sir'           *
      *    *-----------------------------------------------------------*
           copy      "sir/orc/prg/cpy/drefute0.dts"                   .
