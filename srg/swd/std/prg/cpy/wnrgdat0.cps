      *    *===========================================================*
      *    * Determinazione (data) + (nr. giorni) = (data)             *
      *    *-----------------------------------------------------------*
       det-dat-nrg-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-dat-nrg-dtb    to   w-det-dat-nrg-dti      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di incremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-dat-nrg-ngi    =    zero
                     go to det-dat-nrg-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-dat-nrg-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dat-nrg-pgu      .
       det-dat-nrg-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione data di fine mese            *
      *                  *---------------------------------------------*
           move      w-det-dat-nrg-dti    to   s-dat                  .
           move      31                   to   s-gio                  .
       det-dat-nrg-110.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-dat-nrg-110.
      *                  *---------------------------------------------*
      *                  * Aggiornamento data incrementata             *
      *                  *---------------------------------------------*
           move      s-dat                to   w-det-dat-nrg-dti      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-dat-nrg-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-dat-nrg-fpp    =    spaces
                     go to det-dat-nrg-122
           else      go to det-dat-nrg-124.
       det-dat-nrg-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-dat-nrg-fpp      .
           subtract  w-det-dat-nrg-dbg    from w-det-dat-nrg-dig
                                        giving w-det-dat-nrg-ngu      .
           go to     det-dat-nrg-130.
       det-dat-nrg-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
           move      w-det-dat-nrg-dig    to   w-det-dat-nrg-ngu      .
           go to     det-dat-nrg-130.
       det-dat-nrg-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-dat-nrg-ngu    to   w-det-dat-nrg-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di incremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-dat-nrg-pgu    not  < w-det-dat-nrg-ngi
                     go to det-dat-nrg-150.
      *                  *---------------------------------------------*
      *                  * Incremento mese/anno                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-dat-nrg-dim      .
           if        w-det-dat-nrg-dim    >    12
                     move  1              to   w-det-dat-nrg-dim
                     add   1              to   w-det-dat-nrg-dia      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-dat-nrg-100.
       det-dat-nrg-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-dat-nrg-ngi    from w-det-dat-nrg-pgu
                                        giving w-det-dat-nrg-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data incrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           subtract  w-det-dat-nrg-ngu    from w-det-dat-nrg-dig      .
       det-dat-nrg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione (data) - (nr. giorni) = (data)             *
      *    *-----------------------------------------------------------*
       det-nrg-dat-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-nrg-dat-dtb    to   w-det-nrg-dat-dtd      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di decremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-ngd    =    zero
                     go to det-nrg-dat-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-nrg-dat-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nrg-dat-pgu      .
       det-nrg-dat-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura data di inizio mese               *
      *                  *---------------------------------------------*
           move      01                   to   w-det-nrg-dat-ddg      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-nrg-dat-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-nrg-dat-fpp    =    spaces
                     go to det-nrg-dat-122
           else      go to det-nrg-dat-124.
       det-nrg-dat-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-nrg-dat-fpp      .
           subtract  w-det-nrg-dat-ddg    from w-det-nrg-dat-dbg
                                        giving w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero giorni del    *
      *                          * mese                                *
      *                          *-------------------------------------*
           move      w-det-nrg-dat-dtd    to   s-dat                  .
           move      31                   to   s-gio                  .
       det-nrg-dat-125.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-nrg-dat-125.
           move      s-gio                to   w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di decremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-nrg-dat-pgu    not  < w-det-nrg-dat-ngd
                     go to det-nrg-dat-150.
      *                  *---------------------------------------------*
      *                  * Decremento mese/anno                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-det-nrg-dat-ddm      .
           if        w-det-nrg-dat-ddm    =    zero
                     move  12             to   w-det-nrg-dat-ddm
                     subtract 1           from w-det-nrg-dat-dda      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-nrg-dat-100.
       det-nrg-dat-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-nrg-dat-ngd    from w-det-nrg-dat-pgu
                                        giving w-det-nrg-dat-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data decrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-ddg      .
       det-nrg-dat-999.
           exit.

      *    *===========================================================*
      *    * Determinazione in numero giorni tra due date              *
      *    *-----------------------------------------------------------*
       det-dif-dat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dif-dat-gio      .
      *              *-------------------------------------------------*
      *              * Se manca una delle due date : uscita            *
      *              *-------------------------------------------------*
           if        w-det-dif-dat-min    =    zero or
                     w-det-dif-dat-max    =    zero
                     go to  det-dif-dat-900.
      *              *-------------------------------------------------*
      *              * Data iniziale in data di comodo                 *
      *              *-------------------------------------------------*
           move      w-det-dif-dat-min    to   w-det-dif-dat-wrk      .
       det-dif-dat-200.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto con la data incrementata          *
      *                  *---------------------------------------------*
           if        w-det-dif-dat-wrk    =    w-det-dif-dat-max
                     go to  det-dif-dat-900.
                     
           add       1                    to   w-det-dif-dat-gio      .
           move      w-det-dif-dat-min    to   w-det-dat-nrg-dtb      .
           move      w-det-dif-dat-gio    to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   w-det-dif-dat-wrk      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-dif-dat-200.
       det-dif-dat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-dif-dat-999.
       det-dif-dat-999.
           exit.

