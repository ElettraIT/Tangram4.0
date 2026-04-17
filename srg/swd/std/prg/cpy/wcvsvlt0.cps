      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-aav = Ammontare da convertire, espres- *
      *    *                          so nell'altra valuta             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-avb = Ammontare convertito, espresso   *
      *    *                          nella valuta base                *
      *    *                                                           *
      *    * N.B.   : Con arrotondamento del risultato                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-alt-vlb-000.
      *              *-------------------------------------------------*
      *              * Se ammontare da convertire, espresso nell'altra *
      *              * valuta, pari a zero : uscita con ammontare con- *
      *              * vertito, espresso in valuta base, a zero        *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-aav        =    zero
                     move  zero           to   w-cvs-vlt-avb
                     go to cvs-alt-vlb-999.
      *              *-------------------------------------------------*
      *              * Se il coefficiente di cambio dell'altra valuta  *
      *              * e' pari a zero o a uno : uscita con valore con- *
      *              * vertito, espresso in valuta base,  pari all'am- *
      *              * montare da convertire, espresso nell'altra va-  *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-cdc        =    zero or
                     w-cvs-vlt-cdc        =    1
                     move  w-cvs-vlt-aav  to   w-cvs-vlt-avb
                     go to cvs-alt-vlb-999.
      *              *-------------------------------------------------*
      *              * Se la sigla dell'altra valuta e' pari alla si-  *
      *              * gla della valuta base, oppure e' a Spaces : u-  *
      *              * scita con valore convertito, espresso in valuta *
      *              * base, pari all'ammontare da convertire, espres- *
      *              * so nell'altra valuta                            *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-sgl        =    c-sgl  or
                     w-cvs-vlt-sgl        =    spaces
                     move  w-cvs-vlt-aav  to   w-cvs-vlt-avb
                     go to cvs-alt-vlb-999.
      *              *-------------------------------------------------*
      *              * Controllo formale ed eventuale normalizzazione  *
      *              * del numero di decimali dell'altra valuta        *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-dec        >    3
                     move  3              to   w-cvs-vlt-dec          .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di coefficiente   *
      *              * di cambio dell'altra valuta                     *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-tdc        =    "*"
                     go to cvs-alt-vlb-600.
       cvs-alt-vlb-300.
      *              *-------------------------------------------------*
      *              * Se il tipo di coefficiente di cambio dell'altra *
      *              * valuta e' '/'                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   w-cvs-vlt-avb          .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta base           *
      *                  *---------------------------------------------*
           if        c-dec                =    1
                     multiply  10         by   w-cvs-vlt-avb
           else if   c-dec                =    2
                     multiply  100        by   w-cvs-vlt-avb
           else if   c-dec                =    3
                     multiply  1000       by   w-cvs-vlt-avb          .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           multiply  w-cvs-vlt-cdc        by   w-cvs-vlt-avb
                                       rounded                        .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta                *
      *                  *---------------------------------------------*
           if        w-cvs-vlt-dec        =    1
                     divide    10         into w-cvs-vlt-avb
           else if   w-cvs-vlt-dec        =    2
                     divide    100        into w-cvs-vlt-avb
           else if   w-cvs-vlt-dec        =    3
                     divide    1000       into w-cvs-vlt-avb          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cvs-alt-vlb-999.
       cvs-alt-vlb-600.
      *              *-------------------------------------------------*
      *              * Se il tipo di coefficiente di cambio dell'altra *
      *              * valuta e' '*'                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   w-cvs-vlt-avb          .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta base           *
      *                  *---------------------------------------------*
           if        c-dec                =    1
                     multiply  10         by   w-cvs-vlt-avb
           else if   c-dec                =    2
                     multiply  100        by   w-cvs-vlt-avb
           else if   c-dec                =    3
                     multiply  1000       by   w-cvs-vlt-avb          .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           divide    w-cvs-vlt-cdc        into w-cvs-vlt-avb
                                       rounded                        .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta                *
      *                  *---------------------------------------------*
           if        w-cvs-vlt-dec        =    1
                     divide    10         into w-cvs-vlt-avb
           else if   w-cvs-vlt-dec        =    2
                     divide    100        into w-cvs-vlt-avb
           else if   w-cvs-vlt-dec        =    3
                     divide    1000       into w-cvs-vlt-avb          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cvs-alt-vlb-999.
       cvs-alt-vlb-999.
           exit.

      *    *===========================================================*
      *    * Routine di conversione da valuta base ad altra valuta     *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-avb = Ammontare da convertire, espres- *
      *    *                          so nella valuta base             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-aav = Ammontare convertito, espresso   *
      *    *                          nell'altra valuta                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-vlb-alt-000.
      *              *-------------------------------------------------*
      *              * Se ammontare da convertire, espresso nella va-  *
      *              * luta base, pari a zero : uscita con ammontare   *
      *              * convertito, espresso nell'altra valuta, a zero  *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-avb        =    zero
                     move  zero           to   w-cvs-vlt-aav
                     go to cvs-vlb-alt-999.
      *              *-------------------------------------------------*
      *              * Se il coefficiente di cambio dell'altra valuta  *
      *              * e' pari a zero o a uno : uscita con valore con- *
      *              * vertito, espresso nell'altra valuta,  pari al-  *
      *              * l'ammontare da convertire, espresso in valuta   *
      *              * base                                            *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-cdc        =    zero or
                     w-cvs-vlt-cdc        =    1
                     move  w-cvs-vlt-avb  to   w-cvs-vlt-aav
                     go to cvs-vlb-alt-999.
      *              *-------------------------------------------------*
      *              * Se la sigla dell'altra valuta e' pari alla si-  *
      *              * gla della valuta base, oppure e' a Spaces : u-  *
      *              * scita con valore convertito, espresso nell'al-  *
      *              * tra valuta, pari all'ammontare da convertire,   *
      *              * espresso in valuta base                         *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-sgl        =    c-sgl  or
                     w-cvs-vlt-sgl        =    spaces
                     move  w-cvs-vlt-avb  to   w-cvs-vlt-aav
                     go to cvs-vlb-alt-999.
      *              *-------------------------------------------------*
      *              * Controllo formale ed eventuale normalizzazione  *
      *              * del numero di decimali dell'altra valuta        *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-dec        >    3
                     move  3              to   w-cvs-vlt-dec          .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di coefficiente   *
      *              * di cambio dell'altra valuta                     *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-tdc        =    "*"
                     go to cvs-vlb-alt-600.
       cvs-vlb-alt-300.
      *              *-------------------------------------------------*
      *              * Se il tipo di coefficiente di cambio dell'altra *
      *              * valuta e' '/'                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-avb        to   w-cvs-vlt-aav          .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta                *
      *                  *---------------------------------------------*
           if        w-cvs-vlt-dec        =    1
                     multiply  10         by   w-cvs-vlt-aav
           else if   w-cvs-vlt-dec        =    2
                     multiply  100        by   w-cvs-vlt-aav
           else if   w-cvs-vlt-dec        =    3
                     multiply  1000       by   w-cvs-vlt-aav          .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           divide    w-cvs-vlt-cdc        into w-cvs-vlt-aav
                                       rounded                        .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta base           *
      *                  *---------------------------------------------*
           if        c-dec                =    1
                     divide    10         into w-cvs-vlt-aav
           else if   c-dec                =    2
                     divide    100        into w-cvs-vlt-aav
           else if   c-dec                =    3
                     divide    1000       into w-cvs-vlt-aav          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cvs-vlb-alt-999.
       cvs-vlb-alt-600.
      *              *-------------------------------------------------*
      *              * Se il tipo di coefficiente di cambio dell'altra *
      *              * valuta e' '*'                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-avb        to   w-cvs-vlt-aav          .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta base           *
      *                  *---------------------------------------------*
           if        w-cvs-vlt-dec        =    1
                     multiply  10         by   w-cvs-vlt-aav
           else if   w-cvs-vlt-dec        =    2
                     multiply  100        by   w-cvs-vlt-aav
           else if   w-cvs-vlt-dec        =    3
                     multiply  1000       by   w-cvs-vlt-aav          .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           multiply  w-cvs-vlt-cdc        by   w-cvs-vlt-aav
                                       rounded                        .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali valuta                *
      *                  *---------------------------------------------*
           if        c-dec                =    1
                     divide    10         into w-cvs-vlt-aav
           else if   c-dec                =    2
                     divide    100        into w-cvs-vlt-aav
           else if   c-dec                =    3
                     divide    1000       into w-cvs-vlt-aav          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cvs-vlb-alt-999.
       cvs-vlb-alt-999.
           exit.

      *    *===========================================================*
      *    * Routine di conversione da valuta a valuta                 *
      *    *                                                           *
      *    * Input  : Valuta da trasformare                            *
      *    *                                                           *
      *    *            - w-cvs-vdt-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vdt-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vdt-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vdt-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    *            - w-cvs-vdt-amm = Ammontare da convertire,     *
      *    *                              espresso nella valuta da     *
      *    *                              trasformare                  *
      *    *                                                           *
      *    *          Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vnu-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vnu-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vnu-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    * Output : Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-amm = Ammontare convertito,        *
      *    *                              espresso nella valuta        *
      *    *                              in cui trasformare           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-vlt-vlt-000.
      *              *-------------------------------------------------*
      *              * Se i seguenti parametri delle due valute:       *
      *              *  - Sigla valuta                                 *
      *              *  - Decimali valuta                              *
      *              *  - Tipo di coefficiente                         *
      *              * sono uguali : si pone semplicemente l'ammontare *
      *              * in output pari a quello in input, e si esce     *
      *              *-------------------------------------------------*
           if        w-cvs-vdt-sgl        =    w-cvs-vnu-sgl and
                     w-cvs-vdt-dec        =    w-cvs-vnu-dec and
                     w-cvs-vdt-tdc        =    w-cvs-vnu-tdc
                     move  w-cvs-vdt-amm  to   w-cvs-vnu-amm
                     go to cvs-vlt-vlt-999.
      *              *-------------------------------------------------*
      *              * In ogni altro caso si trasforma la valuta in    *
      *              * input in valuta base, e poi il risultato co-    *
      *              * si' ottenuto in valuta di output                *
      *              *-------------------------------------------------*
           move      w-cvs-vdt-sgl        to   w-cvs-vlt-sgl          .
           move      w-cvs-vdt-dec        to   w-cvs-vlt-dec          .
           move      w-cvs-vdt-tdc        to   w-cvs-vlt-tdc          .
           move      w-cvs-vdt-cdc        to   w-cvs-vlt-cdc          .
           move      w-cvs-vdt-amm        to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *
           move      w-cvs-vnu-sgl        to   w-cvs-vlt-sgl          .
           move      w-cvs-vnu-dec        to   w-cvs-vlt-dec          .
           move      w-cvs-vnu-tdc        to   w-cvs-vlt-tdc          .
           move      w-cvs-vnu-cdc        to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
       cvs-vlt-vlt-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione coefficiente di cambio dati un  *
      *    * ammontare in valuta ed un ammontare in valuta base        *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-avb = Ammontare in valuta base         *
      *    *                                                           *
      *    *          w-cvs-vlt-aav = Ammontare in altra valuta        *
      *    *                                                           *
      *    * Output : w-cvs-vlt-cdc = Coefficiente di cambio determi-  *
      *    *                          nato                             *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-cdc-aab-000.
      *              *-------------------------------------------------*
      *              * Se ammontare in valuta a zero : uscita con      *
      *              * coefficiente di cambio a zero                   *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-aav        =    zero
                     move  zero           to   w-cvs-vlt-cdc
                     go to cvs-cdc-aab-999.
      *              *-------------------------------------------------*
      *              * Se ammontare in valuta base a zero : uscita con *
      *              * coefficiente di cambio a zero                   *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-avb        =    zero
                     move  zero           to   w-cvs-vlt-cdc
                     go to cvs-cdc-aab-999.
      *              *-------------------------------------------------*
      *              * Se la sigla dell'altra valuta e' pari alla si-  *
      *              * gla della valuta base, oppure e' a Spaces : u-  *
      *              * scita con coefficiente di cambio pari a quello  *
      *              * della valuta base                               *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-sgl        =    c-sgl  or
                     w-cvs-vlt-sgl        =    spaces
                     move  c-cdc          to   w-cvs-vlt-cdc
                     go to cvs-cdc-aab-999.
      *              *-------------------------------------------------*
      *              * Controllo formale ed eventuale normalizzazione  *
      *              * del numero di decimali dell'altra valuta        *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-dec        >    3
                     move  3              to   w-cvs-vlt-dec          .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di coefficiente   *
      *              * di cambio dell'altra valuta                     *
      *              *-------------------------------------------------*
           if        w-cvs-vlt-tdc        =    "*"
                     go to cvs-cdc-aab-600.
       cvs-cdc-aab-300.
      *              *-------------------------------------------------*
      *              * Se il tipo di coefficiente di cambio dell'altra *
      *              * valuta e' '/'                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-avb        to   w-cvs-vlt-s17          .
           if        w-cvs-vlt-dec        =    1
                     multiply   10        by   w-cvs-vlt-s17
           else if   w-cvs-vlt-dec        =    2
                     multiply  100        by   w-cvs-vlt-s17
           else if   w-cvs-vlt-dec        =    3
                     multiply 1000        by   w-cvs-vlt-s17          .
           divide    w-cvs-vlt-aav        into w-cvs-vlt-s17
                                        giving w-cvs-vlt-cdc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cvs-cdc-aab-999.
       cvs-cdc-aab-600.
      *              *-------------------------------------------------*
      *              * Se il tipo di coefficiente di cambio dell'altra *
      *              * valuta e' '*'                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-avb        to   w-cvs-vlt-s17          .
           if        w-cvs-vlt-dec        =    1
                     multiply   10        by   w-cvs-vlt-s17
           else if   w-cvs-vlt-dec        =    2
                     multiply  100        by   w-cvs-vlt-s17
           else if   w-cvs-vlt-dec        =    3
                     multiply 1000        by   w-cvs-vlt-s17          .
           multiply  w-cvs-vlt-aav        by   w-cvs-vlt-s17
                                        giving w-cvs-vlt-cdc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cvs-cdc-aab-999.
       cvs-cdc-aab-999.
           exit.

