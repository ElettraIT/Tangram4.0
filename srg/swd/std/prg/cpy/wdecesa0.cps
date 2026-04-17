      *    *===========================================================*
      *    * Routine di decodifica da binario a esadecimale            *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : - w-dec-esa-dec-num = numero decimale            *
      *    *                                                           *
      *    * Output : - w-dec-esa-dec-esa = valore esadecimale         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dec-esa-cnv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      "0000"               to   w-dec-esa-dec-esa      .
           move      zero                 to   w-dec-esa-dec-res      .
           move      zero                 to   w-dec-esa-dec-rem      .
           move      zero                 to   w-dec-esa-dec-wcn      .
           move      zero                 to   w-dec-esa-dec-wix      .
           move      5                    to   w-dec-esa-dec-inx      .
       dec-esa-cnv-100.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-dec-esa-dec-num    >    w-dec-esa-dec-max
                     move  spaces         to   w-dec-esa-dec-esa
                     go to dec-esa-cnv-600.
           move      w-dec-esa-dec-num    to   w-dec-esa-dec-wnm      .
       dec-esa-cnv-200.
      *              *-------------------------------------------------*
      *              * Ciclo di divisione                              *
      *              *-------------------------------------------------*
           divide    w-dec-esa-dec-div    into w-dec-esa-dec-wnm
                                        giving w-dec-esa-dec-res
                                     remainder w-dec-esa-dec-rem      .
       dec-esa-cnv-250.
      *              *-------------------------------------------------*
      *              * Test sul resto                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-dec-esa-dec-wnm    <    w-dec-esa-dec-div
                     go to dec-esa-cnv-300
           else      go to dec-esa-cnv-400.
       dec-esa-cnv-300.
      *                  *---------------------------------------------*
      *                  * Se < 16                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assegnazione                            *
      *                      *-----------------------------------------*
           perform   dec-esa-cnv-800      thru dec-esa-cnv-809        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dec-esa-cnv-600.
       dec-esa-cnv-400.
      *                  *---------------------------------------------*
      *                  * Se non < 16                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento residuo                   *
      *                      *-----------------------------------------*
           multiply  w-dec-esa-dec-res    by   w-dec-esa-dec-div
                                        giving w-dec-esa-dec-wcn      .
      *                      *-----------------------------------------*
      *                      * Assegnazione                            *
      *                      *-----------------------------------------*
           perform   dec-esa-cnv-800      thru dec-esa-cnv-809        .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     dec-esa-cnv-200.
       dec-esa-cnv-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-esa-cnv-999.
       dec-esa-cnv-800.
      *              *=================================================*
      *              * Subroutine di assegnazione elemento             *
      *              *-------------------------------------------------*
           move      w-dec-esa-dec-rem    to   w-dec-esa-dec-wix      .
           add       1                    to   w-dec-esa-dec-wix      .
           subtract  1                    from w-dec-esa-dec-inx      .
           move      w-dec-esa-dec-tbx
                    (w-dec-esa-dec-wix)   to   w-dec-esa-dec-ele
                                              (w-dec-esa-dec-inx)     .
           move      w-dec-esa-dec-res    to   w-dec-esa-dec-wnm      .
       dec-esa-cnv-809.
           exit.
       dec-esa-cnv-999.
           exit.

