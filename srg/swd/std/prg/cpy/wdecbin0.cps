      *    *===========================================================*
      *    * Routine di decodifica da decimale a binario 8 bit         *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * - valore massimo convertito '255'                         *
      *    *                                                           *
      *    * - formato esadecimale in output 'XXXXXXXX'                *
      *    *                                                           *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-dec-bin-dec-num = numero decimale              *
      *    *                                                           *
      *    * Output : w-dec-bin-dec-bin = rappresentazione binaria     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dec-bin-cnv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      "00000000"           to   w-dec-bin-dec-bin      .
           move      zero                 to   w-dec-bin-dec-res      .
           move      zero                 to   w-dec-bin-dec-rem      .
           move      zero                 to   w-dec-bin-dec-wcn      .
           move      zero                 to   w-dec-bin-dec-wix      .
           move      9                    to   w-dec-bin-dec-inx      .
       dec-bin-cnv-100.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-dec-bin-dec-num    >    w-dec-bin-dec-max
                     move  spaces         to   w-dec-bin-dec-bin
                     go to dec-bin-cnv-600.
           move      w-dec-bin-dec-num    to   w-dec-bin-dec-wnm      .
       dec-bin-cnv-200.
      *              *-------------------------------------------------*
      *              * Ciclo di divisione                              *
      *              *-------------------------------------------------*
           divide    w-dec-bin-dec-div    into w-dec-bin-dec-wnm
                                        giving w-dec-bin-dec-res
                                     remainder w-dec-bin-dec-rem      .
       dec-bin-cnv-250.
      *              *-------------------------------------------------*
      *              * Test sul resto                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-dec-bin-dec-wnm    <    w-dec-bin-dec-div
                     go to dec-bin-cnv-300
           else      go to dec-bin-cnv-400.
       dec-bin-cnv-300.
      *                  *---------------------------------------------*
      *                  * Se < 16                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assegnazione                            *
      *                      *-----------------------------------------*
           perform   dec-bin-cnv-800      thru dec-bin-cnv-809        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dec-bin-cnv-600.
       dec-bin-cnv-400.
      *                  *---------------------------------------------*
      *                  * Se non < 16                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento residuo                   *
      *                      *-----------------------------------------*
           multiply  w-dec-bin-dec-res    by   w-dec-bin-dec-div
                                        giving w-dec-bin-dec-wcn      .
      *                      *-----------------------------------------*
      *                      * Assegnazione                            *
      *                      *-----------------------------------------*
           perform   dec-bin-cnv-800      thru dec-bin-cnv-809        .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     dec-bin-cnv-200.
       dec-bin-cnv-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-bin-cnv-999.
       dec-bin-cnv-800.
      *              *=================================================*
      *              * Subroutine di assegnazione elemento             *
      *              *-------------------------------------------------*
           move      w-dec-bin-dec-rem    to   w-dec-bin-dec-wix      .
           add       1                    to   w-dec-bin-dec-wix      .
           subtract  1                    from w-dec-bin-dec-inx      .
           move      w-dec-bin-dec-tbx
                    (w-dec-bin-dec-wix)   to   w-dec-bin-dec-ele
                                              (w-dec-bin-dec-inx)     .
           move      w-dec-bin-dec-res    to   w-dec-bin-dec-wnm      .
       dec-bin-cnv-809.
           exit.
       dec-bin-cnv-999.
           exit.

      *    *===========================================================*
      *    * Routine di decodifica da binario a decimale a 8 bit       *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-bin-dec-bin-bin = numero binario               *
      *    *                                                           *
      *    * Output : w-bin-dec-bin-dec = decimale                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       bin-dec-cnv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-bin-dec-bin-dec      .
           move      zero                 to   w-bin-dec-bin-inx      .
           move      zero                 to   w-bin-dec-bin-res      .
       bin-dec-cnv-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-bin-dec-bin-bin    =    spaces
                     go to dec-bin-cnv-600.
       bin-dec-cnv-200.
      *              *-------------------------------------------------*
      *              * Ciclo di calcolo                                *
      *              *-------------------------------------------------*
           add       1                    to   w-bin-dec-bin-inx      .
           if        w-bin-dec-bin-inx    >    w-bin-dec-bin-max
                     go to bin-dec-cnv-600.
           if        w-bin-dec-bin-ele
                    (w-bin-dec-bin-inx)   =    zero or
                     w-bin-dec-bin-ele
                    (w-bin-dec-bin-inx)   >    1
                     go to bin-dec-cnv-200.
      *              *-------------------------------------------------*
      *              * Elevazione a potenza                            *
      *              *-------------------------------------------------*
           perform   bin-dec-cnv-800      thru bin-dec-cnv-809        .
           add       w-bin-dec-bin-res    to   w-bin-dec-bin-dec      .
      *              *-------------------------------------------------*
      *              * A riciclo                                       *
      *              *-------------------------------------------------*
           go to     bin-dec-cnv-200.
       bin-dec-cnv-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     bin-dec-cnv-999.
       bin-dec-cnv-800.
      *              *=================================================*
      *              * Subroutine di elevazione a potenza              *
      *              *-------------------------------------------------*
           move      w-bin-dec-bin-elv    to   w-bin-dec-bin-res      .
           move      zero                 to   w-bin-dec-bin-add      .
           move      w-bin-dec-bin-max    to   w-bin-dec-bin-xel      .
           subtract  w-bin-dec-bin-inx    from w-bin-dec-bin-xel      .
      *
           if        w-bin-dec-bin-xel    =    zero
                     move  1              to   w-bin-dec-bin-res
                     go to bin-dec-cnv-809.
      *
           if        w-bin-dec-bin-xel    =    1
                     move  w-bin-dec-bin-elv
                                          to   w-bin-dec-bin-res
                     go to bin-dec-cnv-809.
       bin-dec-cnv-802.
           if        w-bin-dec-bin-xel    =    1
                     go to bin-dec-cnv-809.
           multiply  w-bin-dec-bin-elv    by   w-bin-dec-bin-res
                                        giving w-bin-dec-bin-add      .
           move      w-bin-dec-bin-add    to   w-bin-dec-bin-res      .
           subtract  1                    from w-bin-dec-bin-xel      .
           go to     bin-dec-cnv-802.
       bin-dec-cnv-809.
           exit.
       bin-dec-cnv-999.
           exit.

      *    *===========================================================*
      *    * Routine di somma 'xor' tra binari                         *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-bin-xor-bin-bi1 = numero binario 1             *
      *    *          w-bin-xor-bin-bi2 = numero binario 2             *
      *    *                                                           *
      *    * Output : w-bin-xor-bin-bin = numero binario risultante    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       bin-xor-bin-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      "00000000"           to   w-bin-xor-bin-bin      .
           move      zero                 to   w-bin-xor-bin-inx      .
       bin-xor-bin-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-bin-xor-bin-bi1    =    spaces or
                     w-bin-xor-bin-bi2    =    spaces
                     go to bin-xor-bin-900.
       bin-xor-bin-200.
      *              *-------------------------------------------------*
      *              * Ciclo di calcolo                                *
      *              *-------------------------------------------------*
           add       1                    to   w-bin-xor-bin-inx      .
           if        w-bin-xor-bin-inx    >    w-bin-xor-bin-max
                     go to bin-xor-bin-900.
      *              *-------------------------------------------------*
      *              * 'xor'                                           *
      *              *-------------------------------------------------*
           if        w-bin-xor-bin-el1
                    (w-bin-xor-bin-inx)   =    1 and
                     w-bin-xor-bin-el2
                    (w-bin-xor-bin-inx)   =    1
                     move  "0"            to   w-bin-xor-bin-ele
                                              (w-bin-xor-bin-inx)
           else if   w-bin-xor-bin-el1
                    (w-bin-xor-bin-inx)   =    0 and
                     w-bin-xor-bin-el2
                    (w-bin-xor-bin-inx)   =    0
                     move  "0"            to   w-bin-xor-bin-ele
                                              (w-bin-xor-bin-inx)
           else if   w-bin-xor-bin-el1
                    (w-bin-xor-bin-inx)   =    1 and
                     w-bin-xor-bin-el2
                    (w-bin-xor-bin-inx)   =    0
                     move  "1"            to   w-bin-xor-bin-ele
                                              (w-bin-xor-bin-inx)
           else if   w-bin-xor-bin-el1
                    (w-bin-xor-bin-inx)   =    0 and
                     w-bin-xor-bin-el2
                    (w-bin-xor-bin-inx)   =    1
                     move  "1"            to   w-bin-xor-bin-ele
                                              (w-bin-xor-bin-inx)     .
      *              *-------------------------------------------------*
      *              * A riciclo                                       *
      *              *-------------------------------------------------*
           go to     bin-xor-bin-200.
       bin-xor-bin-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     bin-xor-bin-999.
       bin-xor-bin-999.
           exit.

      *    *===========================================================*
      *    * Routine di decodifica da decimale a binario 16 bit        *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * - valore massimo convertito '65535'                       *
      *    *                                                           *
      *    * - formato esadecimale in output 'XXXXXXXX'                *
      *    *                                                           *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-dec-b16-dec-num = numero decimale              *
      *    *                                                           *
      *    * Output : w-dec-b16-dec-b16 = rappresentazione binaria     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dec-b16-cnv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      "0000000000000000"
                                          to   w-dec-b16-dec-b16      .
           move      zero                 to   w-dec-b16-dec-res      .
           move      zero                 to   w-dec-b16-dec-rem      .
           move      zero                 to   w-dec-b16-dec-wcn      .
           move      zero                 to   w-dec-b16-dec-wix      .
           move      17                   to   w-dec-b16-dec-inx      .
       dec-b16-cnv-100.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-dec-b16-dec-num    >    w-dec-b16-dec-max
                     move  spaces         to   w-dec-b16-dec-b16
                     go to dec-b16-cnv-600.
           move      w-dec-b16-dec-num    to   w-dec-b16-dec-wnm      .
       dec-b16-cnv-200.
      *              *-------------------------------------------------*
      *              * Ciclo di divisione                              *
      *              *-------------------------------------------------*
           divide    w-dec-b16-dec-div    into w-dec-b16-dec-wnm
                                        giving w-dec-b16-dec-res
                                     remainder w-dec-b16-dec-rem      .
       dec-b16-cnv-250.
      *              *-------------------------------------------------*
      *              * Test sul resto                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-dec-b16-dec-wnm    <    w-dec-b16-dec-div
                     go to dec-b16-cnv-300
           else      go to dec-b16-cnv-400.
       dec-b16-cnv-300.
      *                  *---------------------------------------------*
      *                  * Se < 16                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assegnazione                            *
      *                      *-----------------------------------------*
           perform   dec-b16-cnv-800      thru dec-b16-cnv-809        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dec-b16-cnv-600.
       dec-b16-cnv-400.
      *                  *---------------------------------------------*
      *                  * Se non < 16                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento residuo                   *
      *                      *-----------------------------------------*
           multiply  w-dec-b16-dec-res    by   w-dec-b16-dec-div
                                        giving w-dec-b16-dec-wcn      .
      *                      *-----------------------------------------*
      *                      * Assegnazione                            *
      *                      *-----------------------------------------*
           perform   dec-b16-cnv-800      thru dec-b16-cnv-809        .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     dec-b16-cnv-200.
       dec-b16-cnv-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-b16-cnv-999.
       dec-b16-cnv-800.
      *              *=================================================*
      *              * Subroutine di assegnazione elemento             *
      *              *-------------------------------------------------*
           move      w-dec-b16-dec-rem    to   w-dec-b16-dec-wix      .
           add       1                    to   w-dec-b16-dec-wix      .
           subtract  1                    from w-dec-b16-dec-inx      .
           move      w-dec-b16-dec-tbx
                    (w-dec-b16-dec-wix)   to   w-dec-b16-dec-ele
                                              (w-dec-b16-dec-inx)     .
           move      w-dec-b16-dec-res    to   w-dec-b16-dec-wnm      .
       dec-b16-cnv-809.
           exit.
       dec-b16-cnv-999.
           exit.

      *    *===========================================================*
      *    * Routine di decodifica da binario a decimale a 16 bit      *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-b16-dec-b16-bin = numero binario               *
      *    *                                                           *
      *    * Output : w-b16-dec-b16-dec = decimale                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       b16-dec-cnv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-b16-dec-b16-dec      .
           move      zero                 to   w-b16-dec-b16-inx      .
           move      zero                 to   w-b16-dec-b16-res      .
       b16-dec-cnv-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-b16-dec-b16-bin    =    spaces
                     go to dec-bin-cnv-600.
       b16-dec-cnv-200.
      *              *-------------------------------------------------*
      *              * Ciclo di calcolo                                *
      *              *-------------------------------------------------*
           add       1                    to   w-b16-dec-b16-inx      .
           if        w-b16-dec-b16-inx    >    w-b16-dec-b16-max
                     go to b16-dec-cnv-600.
           if        w-b16-dec-b16-ele
                    (w-b16-dec-b16-inx)   =    zero or
                     w-b16-dec-b16-ele
                    (w-b16-dec-b16-inx)   >    1
                     go to b16-dec-cnv-200.
      *              *-------------------------------------------------*
      *              * Elevazione a potenza                            *
      *              *-------------------------------------------------*
           perform   b16-dec-cnv-800      thru b16-dec-cnv-809        .
           add       w-b16-dec-b16-res    to   w-b16-dec-b16-dec      .
      *              *-------------------------------------------------*
      *              * A riciclo                                       *
      *              *-------------------------------------------------*
           go to     b16-dec-cnv-200.
       b16-dec-cnv-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     b16-dec-cnv-999.
       b16-dec-cnv-800.
      *              *=================================================*
      *              * Subroutine di elevazione a potenza              *
      *              *-------------------------------------------------*
           move      w-b16-dec-b16-elv    to   w-b16-dec-b16-res      .
           move      zero                 to   w-b16-dec-b16-add      .
           move      w-b16-dec-b16-max    to   w-b16-dec-b16-xel      .
           subtract  w-b16-dec-b16-inx    from w-b16-dec-b16-xel      .
      *
           if        w-b16-dec-b16-xel    =    zero
                     move  1              to   w-b16-dec-b16-res
                     go to b16-dec-cnv-809.
      *
           if        w-b16-dec-b16-xel    =    1
                     move  w-b16-dec-b16-elv
                                          to   w-b16-dec-b16-res
                     go to b16-dec-cnv-809.
       b16-dec-cnv-802.
           if        w-b16-dec-b16-xel    =    1
                     go to b16-dec-cnv-809.
           multiply  w-b16-dec-b16-elv    by   w-b16-dec-b16-res
                                        giving w-b16-dec-b16-add      .
           move      w-b16-dec-b16-add    to   w-b16-dec-b16-res      .
           subtract  1                    from w-b16-dec-b16-xel      .
           go to     b16-dec-cnv-802.
       b16-dec-cnv-809.
           exit.
       b16-dec-cnv-999.
           exit.

      *    *===========================================================*
      *    * Routine di somma 'xor' tra binari a 16 bit                *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-b16-xor-b16-bi1 = numero binario 1             *
      *    *          w-b16-xor-b16-bi2 = numero binario 2             *
      *    *                                                           *
      *    * Output : w-b16-xor-b16-bin = numero binario risultante    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       b16-xor-b16-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      "00000000"           to   w-b16-xor-b16-bin      .
           move      zero                 to   w-b16-xor-b16-inx      .
       b16-xor-b16-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-b16-xor-b16-bi1    =    spaces or
                     w-b16-xor-b16-bi2    =    spaces
                     go to b16-xor-b16-900.
       b16-xor-b16-200.
      *              *-------------------------------------------------*
      *              * Ciclo di calcolo                                *
      *              *-------------------------------------------------*
           add       1                    to   w-b16-xor-b16-inx      .
           if        w-b16-xor-b16-inx    >    w-b16-xor-b16-max
                     go to b16-xor-b16-900.
      *              *-------------------------------------------------*
      *              * 'xor'                                           *
      *              *-------------------------------------------------*
           if        w-b16-xor-b16-el1
                    (w-b16-xor-b16-inx)   =    1 and
                     w-b16-xor-b16-el2
                    (w-b16-xor-b16-inx)   =    1
                     move  "0"            to   w-b16-xor-b16-ele
                                              (w-b16-xor-b16-inx)
           else if   w-b16-xor-b16-el1
                    (w-b16-xor-b16-inx)   =    0 and
                     w-b16-xor-b16-el2
                    (w-b16-xor-b16-inx)   =    0
                     move  "0"            to   w-b16-xor-b16-ele
                                              (w-b16-xor-b16-inx)
           else if   w-b16-xor-b16-el1
                    (w-b16-xor-b16-inx)   =    1 and
                     w-b16-xor-b16-el2
                    (w-b16-xor-b16-inx)   =    0
                     move  "1"            to   w-b16-xor-b16-ele
                                              (w-b16-xor-b16-inx)
           else if   w-b16-xor-b16-el1
                    (w-b16-xor-b16-inx)   =    0 and
                     w-b16-xor-b16-el2
                    (w-b16-xor-b16-inx)   =    1
                     move  "1"            to   w-b16-xor-b16-ele
                                              (w-b16-xor-b16-inx)     .
      *              *-------------------------------------------------*
      *              * A riciclo                                       *
      *              *-------------------------------------------------*
           go to     b16-xor-b16-200.
       b16-xor-b16-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     b16-xor-b16-999.
       b16-xor-b16-999.
           exit.

