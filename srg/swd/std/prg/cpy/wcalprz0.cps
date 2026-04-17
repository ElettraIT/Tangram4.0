      *    *===========================================================*
      *    * Determinazione prezzo netto                               *
      *    *                                                           *
      *    * Input  : w-cal-prz-net-prz     = prezzo lordo             *
      *    *          w-cal-imp-sco-psc (n) = sconti                   *
      *    *                                                           *
      *    * Output : w-cal-prz-net-prz     = prezzo netto             *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-prz-net-000.
      *              *-------------------------------------------------*
      *              * Se valore in input a zero : uscita              *
      *              *-------------------------------------------------*
           if        w-cal-prz-net-prz    =    zero
                     go to cal-prz-net-999.
       cal-prz-net-050.
      *              *-------------------------------------------------*
      *              * Se percentuali di sconto tutte a zero, prezzo   *
      *              * netto pari al valore in input : uscita          *
      *              *-------------------------------------------------*
           if        w-cal-prz-net-psc (1)
                                          =    zero and
                     w-cal-prz-net-psc (2)
                                          =    zero and
                     w-cal-prz-net-psc (3)
                                          =    zero and
                     w-cal-prz-net-psc (4)
                                          =    zero and
                     w-cal-prz-net-psc (5)
                                          =    zero
                     go to cal-prz-net-999.
       cal-prz-net-100.
      *              *-------------------------------------------------*
      *              * Ciclo per abbattimento con sconti               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-cal-prz-net-ctr      .
       cal-prz-net-200.
           add       1                    to   w-cal-prz-net-ctr      .
           if        w-cal-prz-net-ctr    >    5
                     go to cal-prz-net-800.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di calcolo                 *
      *                  *---------------------------------------------*
           move      w-cal-prz-net-prz    to   w-cal-imp-sco-iml      .
           move      w-cal-prz-net-psc
                    (w-cal-prz-net-ctr)   to   w-cal-imp-sco-psc      .
           perform   cal-imp-sco-000      thru cal-imp-sco-999        .
           move      w-cal-imp-sco-imn    to   w-cal-prz-net-prz      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cal-prz-net-200.
       cal-prz-net-800.
       cal-prz-net-999.
           exit.

      *    *===========================================================*
      *    * Routine di calcolo importo scontato                       *
      *    *                                                           *
      *    * Input  : w-cal-imp-sco-iml = importo lordo da scontare    *
      *    *          w-cal-imp-sco-psc = percentuale di sconto        *
      *    *                                                           *
      *    * Output : w-cal-imp-sco-imn = importo netto scontato       *
      *    *          w-cal-imp-sco-ams = ammontare dello sconto       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-imp-sco-000.
      *              *-------------------------------------------------*
      *              * Se percentuale di sconto a zero : uscita        *
      *              *-------------------------------------------------*
           if        w-cal-imp-sco-psc    =    zero
                     move  w-cal-imp-sco-iml
                                          to   w-cal-imp-sco-imn
                     move  zero           to   w-cal-imp-sco-ams
                     go to cal-imp-sco-999.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           subtract  100,0                from w-cal-imp-sco-psc
                                        giving w-cal-imp-sco-w01      .
           multiply  w-cal-imp-sco-iml    by   w-cal-imp-sco-w01
                                        giving w-cal-imp-sco-w02      .
           divide    100                  into w-cal-imp-sco-w02
                                        giving w-cal-imp-sco-imn
                                               rounded                .
           subtract  w-cal-imp-sco-imn    from w-cal-imp-sco-iml
                                        giving w-cal-imp-sco-ams      .
       cal-imp-sco-999.
           exit.

